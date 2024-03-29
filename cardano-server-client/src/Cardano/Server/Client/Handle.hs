{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Cardano.Server.Client.Handle where

import           Cardano.Server.Client.Gen          (randomAddressBech32Text)
import           Cardano.Server.Client.Internal     (ClientEndpoint (..), Interval, Mode (..))
import           Cardano.Server.Endpoints.Tx.Submit (SubmitTxReqBody (..))
import           Cardano.Server.Internal            (ServerM, getNetworkId)
import           Cardano.Server.Utils.Logger        (logMsg, logSmth, (.<))
import           Cardano.Server.Utils.Wait          (waitTime)
import           Control.Monad                      (forever, (>=>))
import           Control.Monad.Catch                (Exception, MonadThrow (throwM))
import           Control.Monad.IO.Class             (MonadIO (..))
import           Data.Aeson                         (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy               as LBS
import           Data.Default                       (Default (..))
import           Data.List.Extra                    (chunksOf)
import           Data.Proxy                         (Proxy (..))
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           GHC.Base                           (Symbol)
import           Servant.Client                     (ClientEnv, runClientM)
import           System.Random                      (Random, randomIO, randomRIO)
import           Text.Read                          (readEither)

type HasServantClientEnv = ?servantClientEnv :: ClientEnv

-- Proxy here is necessary to save the user from writing explicit type applications
data ClientHandle api = ClientHandle
    -- Auto
    { autoPing       :: HasServantClientEnv => Interval -> ServerM api (Proxy "ping")
    , autoUtxos      :: HasServantClientEnv => Interval -> ServerM api (Proxy "utxos")
    , autoNewTx      :: HasServantClientEnv => Interval -> ServerM api (Proxy "newTx")
    , autoSumbitTx   :: HasServantClientEnv => Interval -> ServerM api (Proxy "submitTx")
    , autoServerTx   :: HasServantClientEnv => Interval -> ServerM api (Proxy "serverTx")
    , autoVersion    :: HasServantClientEnv => Interval -> ServerM api (Proxy "version")
    -- Manual
    , manualPing     :: HasServantClientEnv => Text -> ServerM api (Proxy "ping")
    , manualUtxos    :: HasServantClientEnv => Text -> ServerM api (Proxy "utxos")
    , manualNewTx    :: HasServantClientEnv => Text -> ServerM api (Proxy "newTx")
    , manualSubmitTx :: HasServantClientEnv => Text -> ServerM api (Proxy "submitTx")
    , manualServerTx :: HasServantClientEnv => Text -> ServerM api (Proxy "serverTx")
    , manualVersion  :: HasServantClientEnv => Text -> ServerM api (Proxy "version")
    }

instance Default (ClientHandle api) where
    def = ClientHandle
        { autoPing       = autoWith (pure ())
        , autoUtxos      = \i -> getNetworkId >>= (`autoWith` i) . randomAddressBech32Text
        , autoNewTx      = throwAutoNotImplemented "newTx"
        , autoSumbitTx   = throwAutoNotImplemented "submitTx"
        , autoServerTx   = throwAutoNotImplemented "serverTx"
        , autoVersion     = throwAutoNotImplemented "version"
        , manualPing     = const $ sendRequest ()
        , manualUtxos    = manualWithRead
        , manualNewTx    = throwManualNotImplemented "newTx"
        , manualSubmitTx = manualWith readSubmitTxArg
        , manualServerTx = throwManualNotImplemented "serverTx"
        , manualVersion  = throwManualNotImplemented "version"
        }
        where
            readSubmitTxArg (T.splitOn "," -> tx:wits)
                = pure $ SubmitTxReqBody tx $ map (\[a,b] -> (a,b)) $ chunksOf 2 wits

data NotImplementedMethodError = NotImplementedMethodError Mode Text
    deriving (Show, Exception)

throwAutoNotImplemented :: Text -> Interval -> ServerM api a
throwAutoNotImplemented e i = throwM $ NotImplementedMethodError (Auto i) e

throwManualNotImplemented :: Text -> Text -> ServerM api a
throwManualNotImplemented e t = throwM $ NotImplementedMethodError (Manual t) e

autoWith :: forall (e :: Symbol) api.
    ( HasServantClientEnv
    , ClientEndpoint e api
    ) => ServerM api (EndpointArg e api) -> Interval -> ServerM api (Proxy e)
autoWith gen averageInterval = forever $ do
    reqBody <- gen
    sendRequest @e reqBody
    waitTime =<< randomRIO (1, averageInterval * 2)

autoWithRandom :: forall (e :: Symbol) api.
    ( HasServantClientEnv
    , ClientEndpoint e api
    , Random (EndpointArg e api)
    ) => Interval -> ServerM api (Proxy e)
autoWithRandom = autoWith (liftIO randomIO)

manualWith :: forall (e :: Symbol) api.
    ( HasServantClientEnv
    , ClientEndpoint e api
    ) => (Text -> ServerM api (EndpointArg e api)) -> Text -> ServerM api (Proxy e)
manualWith = (>=> sendRequest @e)

manualWithRead :: forall (e :: Symbol) api.
    ( HasServantClientEnv
    , ClientEndpoint e api
    , Read (EndpointArg e api)
    ) => Text -> ServerM api (Proxy e)
manualWithRead = either ((Proxy <$) . logSmth) (sendRequest @e) . readEither . T.unpack

manualWithJsonFile :: forall (e :: Symbol) api.
    ( HasServantClientEnv
    , ClientEndpoint e api
    , FromJSON (EndpointArg e api)
    ) => Text -> ServerM api (Proxy e)
manualWithJsonFile filePath
    = liftIO (LBS.readFile $ T.unpack filePath) >>= either ((Proxy <$) . logSmth) (sendRequest @e) . eitherDecode

sendRequest :: forall e api.
    ( HasServantClientEnv
    , ClientEndpoint e api
    ) => EndpointArg e api -> ServerM api (Proxy e)
sendRequest reqBody = Proxy <$ do
    logMsg $ "Sending request with:\n" .< reqBody
    res <- liftIO (flip runClientM ?servantClientEnv $ endpointClient @e @api reqBody)
    logMsg $ "Received response:\n" <> either (T.pack . show) (T.pack . show) res