{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TupleSections #-}

module Cardano.Server.Client.Client where

import           Cardano.Server.Client.Internal (ClientEndpoint, ClientHandle (..), EndpointArg, HasServantClientEnv, Interval,
                                                 Mode (..), NotImplementedMethodError (..), ServerEndpoint (..), endpointClient,
                                                 randomFundsReqBody, randomSubmitTxBody, throwAutoNotImplemented,
                                                 throwManualNotImplemented)
import           Cardano.Server.Client.Opts     (Options (..), runWithOpts)
import           Cardano.Server.Config          (Config (..), loadConfig)
import           Cardano.Server.Internal        (ServerHandle, ServerM, loadEnv, runServerM)
import           Cardano.Server.Main            (port)
import           Cardano.Server.Utils.Logger    (HasLogger (..), logSmth, (.<))
import           Cardano.Server.Utils.Wait      (waitTime)
import           Control.Exception              (handle)
import           Control.Monad.Reader           (MonadIO (..), forever, (>=>), void)
import           Data.Aeson                     (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy           as LBS
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Network.HTTP.Client            (defaultManagerSettings, newManager)
import           Servant.Client                 (BaseUrl (BaseUrl), ClientEnv (..), Scheme (Http), defaultMakeClientRequest,
                                                 runClientM)
import           System.Random                  (Random, randomIO, randomRIO)
import           Text.Read                      (readEither)
import Data.Proxy

runClient :: ServerHandle api -> ClientHandle api -> IO ()
runClient sh ClientHandle{..} = handleNotImplementedMethods $ do
    Options{..} <- runWithOpts
    Config{..}  <- loadConfig
    env         <- loadEnv sh
    manager     <- newManager defaultManagerSettings
    let ?servantClientEnv = ClientEnv
            manager
            (BaseUrl Http "localhost" {- (T.unpack cServerAddress) -} port ""{- (show optsEndpoint) -})
            Nothing
            defaultMakeClientRequest
    runServerM env $ withGreetings $ case (optsMode, optsEndpoint) of
        (Auto     i, PingE    ) -> void $ autoPing         i
        (Auto     i, FundsE   ) -> void $ autoFunds        i
        (Auto     i, NewTxE   ) -> void $ autoNewTx        i
        (Auto     i, SubmitTxE) -> void $ autoSumbitTx     i
        (Auto     i, ServerTxE) -> void $ autoServerTx     i
        (Manual txt, PingE    ) -> void $ manualPing     txt
        (Manual txt, FundsE   ) -> void $ manualFunds    txt
        (Manual txt, NewTxE   ) -> void $ manualNewTx    txt
        (Manual txt, SubmitTxE) -> void $ manualSubmitTx txt
        (Manual txt, ServerTxE) -> void $ manualServerTx txt
    where
        withGreetings = (logMsg "Starting client..." >>)
        handleNotImplementedMethods = handle $ \(NotImplementedMethodError mode endpoint) ->
            let mode' = case mode of {Auto _ -> "auto"; Manual _ -> "manual"}
            in logMsg $ "You are about to use a function from client handle for which you did not provide an implementation:\n"
                <> mode' .< endpoint

defaultHandle :: ClientHandle api
defaultHandle = ClientHandle
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