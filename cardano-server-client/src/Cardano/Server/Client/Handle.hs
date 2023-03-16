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

module Cardano.Server.Client.Handle where

import           Cardano.Server.Client.Gen      (randomFundsReqBody, randomSubmitTxBody)
import           Cardano.Server.Client.Internal (ClientEndpoint (..), Interval, Mode (..), ServerEndpoint (..))
import           Cardano.Server.Internal        (ServerM)
import           Cardano.Server.Utils.Logger    (HasLogger (..), logSmth)
import           Cardano.Server.Utils.Wait      (waitTime)
import           Control.Monad.Catch            (Exception, MonadThrow (throwM))
import           Control.Monad.IO.Class         (MonadIO (..))
import           Control.Monad.Reader           (forever, (>=>))
import           Data.Aeson                     (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy           as LBS
import           Data.Default                   (Default (..))
import           Data.Proxy                     (Proxy (..))
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Servant.Client                 (ClientEnv, runClientM)
import           System.Random                  (Random, randomIO, randomRIO)
import           Text.Read                      (readEither)

type HasServantClientEnv = ?servantClientEnv :: ClientEnv

-- Proxy here is necessary to save the user from writing explicit type applications
data ClientHandle api = ClientHandle
    -- Auto
    { autoPing       :: HasServantClientEnv => Interval -> ServerM api (Proxy 'PingE)
    , autoFunds      :: HasServantClientEnv => Interval -> ServerM api (Proxy 'FundsE)
    , autoNewTx      :: HasServantClientEnv => Interval -> ServerM api (Proxy 'NewTxE)
    , autoSumbitTx   :: HasServantClientEnv => Interval -> ServerM api (Proxy 'SubmitTxE)
    , autoServerTx   :: HasServantClientEnv => Interval -> ServerM api (Proxy 'ServerTxE)
    -- -- Manual
    , manualPing     :: HasServantClientEnv => Text -> ServerM api (Proxy 'PingE)
    , manualFunds    :: HasServantClientEnv => Text -> ServerM api (Proxy 'FundsE)
    , manualNewTx    :: HasServantClientEnv => Text -> ServerM api (Proxy 'NewTxE)
    , manualSubmitTx :: HasServantClientEnv => Text -> ServerM api (Proxy 'SubmitTxE)
    , manualServerTx :: HasServantClientEnv => Text -> ServerM api (Proxy 'ServerTxE)
    }

instance Default (ClientHandle api) where
    def = ClientHandle
        -- Auto
        { autoPing     = autoWith (pure ())
        , autoFunds    = autoWith randomFundsReqBody
        , autoNewTx    = throwAutoNotImplemented NewTxE
        , autoSumbitTx = autoWith randomSubmitTxBody
        , autoServerTx = throwAutoNotImplemented ServerTxE
        -- -- Manual
        , manualPing     = const $ sendRequest ()
        , manualFunds    = manualWithRead
        , manualNewTx    = throwManualNotImplemented NewTxE
        , manualSubmitTx = manualWithRead
        , manualServerTx = throwManualNotImplemented ServerTxE
        }

data NotImplementedMethodError = NotImplementedMethodError Mode ServerEndpoint
    deriving (Show, Exception)

throwAutoNotImplemented :: ServerEndpoint -> Interval -> ServerM api a
throwAutoNotImplemented e i = throwM $ NotImplementedMethodError (Auto i) e

throwManualNotImplemented :: ServerEndpoint -> Text -> ServerM api a
throwManualNotImplemented e t = throwM $ NotImplementedMethodError (Manual t) e

autoWith :: forall (e :: ServerEndpoint) api.
    ( HasServantClientEnv
    , ClientEndpoint e api
    ) => ServerM api (EndpointArg e api) -> Interval -> ServerM api (Proxy e)
autoWith gen averageInterval = forever $ do
    reqBody <- gen
    sendRequest @e reqBody
    waitTime =<< randomRIO (1, averageInterval * 2)

autoWithRandom :: forall (e :: ServerEndpoint) api.
    ( HasServantClientEnv
    , ClientEndpoint e api
    , Random (EndpointArg e api)
    ) => Interval -> ServerM api (Proxy e)
autoWithRandom = autoWith (liftIO randomIO)

manualWith :: forall (e :: ServerEndpoint) api.
    ( HasServantClientEnv
    , ClientEndpoint e api
    ) => (Text -> ServerM api (EndpointArg e api)) -> Text -> ServerM api (Proxy e)
manualWith = (>=> sendRequest @e)

manualWithRead :: forall (e :: ServerEndpoint) api.
    ( HasServantClientEnv
    , ClientEndpoint e api
    , Read (EndpointArg e api)
    ) => Text -> ServerM api (Proxy e)
manualWithRead = either ((Proxy <$) . logSmth) (sendRequest @e) . readEither . T.unpack

manualWithJsonFile :: forall (e :: ServerEndpoint) api.
    ( HasServantClientEnv
    , ClientEndpoint e api
    , FromJSON (EndpointArg e api)
    ) => Text -> ServerM api (Proxy e)
manualWithJsonFile filePath
    = liftIO (LBS.readFile $ T.unpack filePath) >>= either ((Proxy <$) . logSmth) (sendRequest @e) . eitherDecode

sendRequest :: forall e api. (HasServantClientEnv, ClientEndpoint e api) => EndpointArg e api -> ServerM api (Proxy e)
sendRequest reqBody = Proxy <$ do
    res <- liftIO (flip runClientM ?servantClientEnv $ endpointClient @e @api reqBody)
    logMsg $ "Received response:\n" <> either (T.pack . show) (T.pack . show) res