{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

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
import           Control.Monad.Reader           (MonadIO (..), forever)
import           Data.Aeson                     (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy           as LBS
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Network.HTTP.Client            (defaultManagerSettings, newManager)
import           Servant.Client                 (BaseUrl (BaseUrl), ClientEnv (..), Scheme (Http), defaultMakeClientRequest,
                                                 runClientM)
import           System.Random                  (Random, randomIO, randomRIO)
import           Text.Read                      (readEither)

runClient :: ServerHandle api -> ClientHandle api -> IO ()
runClient sh ClientHandle{..} = handle notImplementedMethods $ do
    Options{..} <- runWithOpts
    Config{..}  <- loadConfig
    env         <- loadEnv sh
    manager     <- newManager defaultManagerSettings
    let ?servantClientEnv = ClientEnv
            manager
            (BaseUrl Http (T.unpack cServerAddress) port (show optsEndpoint))
            Nothing
            defaultMakeClientRequest
    runServerM env $ withGreetings $ case (optsMode, optsEndpoint) of
        (Auto     i, PingE    ) -> autoPing         i
        (Auto     i, FundsE   ) -> autoFunds        i
        (Auto     i, NewTxE   ) -> autoNewTx        i
        (Auto     i, SubmitTxE) -> autoSumbitTx     i
        (Auto     i, ServerTxE) -> autoServerTx     i
        (Manual txt, PingE    ) -> manualPing     txt
        (Manual txt, FundsE   ) -> manualFunds    txt
        (Manual txt, NewTxE   ) -> manualNewTx    txt
        (Manual txt, SubmitTxE) -> manualSubmitTx txt
        (Manual txt, ServerTxE) -> manualServerTx txt
    where
        withGreetings = (logMsg "Starting client..." >>)
        notImplementedMethods (NotImplementedMethodError mode endpoint) = 
            let mode' = case mode of {Auto _ -> "auto"; Manual _ -> "manual"}
            in logMsg $ "You are about to use a function from client handle for which you did not provide an implementation:\n"
                <> mode' .< endpoint

defaultHandle :: ClientHandle api
defaultHandle = ClientHandle
    -- Auto
    { autoPing     = autoWithGenerator @'PingE (pure ())
    , autoFunds    = autoWithGenerator @'FundsE randomFundsReqBody
    , autoNewTx    = throwAutoNotImplemented NewTxE
    , autoSumbitTx = autoWithGenerator @'SubmitTxE randomSubmitTxBody
    , autoServerTx = throwAutoNotImplemented ServerTxE
    -- Manual
    , manualPing     = const $ sendRequest @'PingE ()
    , manualFunds    = manualWithRead @'FundsE
    , manualNewTx    = throwManualNotImplemented NewTxE
    , manualSubmitTx = manualWithRead @'SubmitTxE
    , manualServerTx = throwManualNotImplemented ServerTxE
    }

autoWithGenerator :: forall (e :: ServerEndpoint) api.
    ( HasServantClientEnv
    , ClientEndpoint e api
    ) => ServerM api (EndpointArg e api) -> Interval -> ServerM api ()
autoWithGenerator gen averageInterval = forever $ do
    reqBody <- gen
    sendRequest @e reqBody
    waitTime =<< randomRIO (1, averageInterval * 2)

autoWithRandom :: forall (e :: ServerEndpoint) api.
    ( HasServantClientEnv
    , ClientEndpoint e api
    , Random (EndpointArg e api)
    ) => Interval -> ServerM api ()
autoWithRandom = autoWithGenerator @e (liftIO randomIO)

manualWithRead :: forall (e :: ServerEndpoint) api.
    ( HasServantClientEnv
    , ClientEndpoint e api
    , Read (EndpointArg e api)
    ) => Text -> ServerM api ()
manualWithRead = either logSmth (sendRequest @e) . readEither . T.unpack

manualWithJsonFile :: forall (e :: ServerEndpoint) api.
    ( HasServantClientEnv
    , ClientEndpoint e api
    , FromJSON (EndpointArg e api)
    ) => Text -> ServerM api ()
manualWithJsonFile filePath 
    = liftIO (LBS.readFile $ T.unpack filePath) >>= either logSmth (sendRequest @e) . eitherDecode

sendRequest :: forall e api. (HasServantClientEnv, ClientEndpoint e api) => EndpointArg e api -> ServerM api ()
sendRequest reqBody = do
    res <- liftIO (flip runClientM ?servantClientEnv $ endpointClient @e @api reqBody)
    logMsg $ "Received response:\n" <> either (T.pack . show) (T.pack . show) res