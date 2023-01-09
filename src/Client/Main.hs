{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

module Client.Main where

import           Client.Class              (HasClient(..), ClientM, ClientRequestOf, runClientM)
import           Client.Opts               (AutoOptions(..), Maximum, Options(..), runWithOpts)
import           Control.Monad             (replicateM)
import           Control.Monad.Reader      (MonadIO(..), forever, when)
import           Data.Aeson                (encode)
import           Data.List                 (nub)
import qualified Data.Text                 as T
import           Network.HTTP.Client       (httpLbs, defaultManagerSettings, newManager, parseRequest,
                                            Manager, Request(..), RequestBody(..), responseStatus, responseTimeoutMicro)
import           Network.HTTP.Types.Header (hContentType)
import           Network.HTTP.Types.Status (status204)
import           Server.Config             (Config(..), loadConfig)       
import qualified Server.Class              as Server
import           System.Random             (randomRIO)
import           Utils.Logger              (HasLogger(..), (.<))
import           Utils.Wait                (waitTime)

startClient :: forall s. HasClient s => IO ()
startClient = do
    opts <- runWithOpts @s
    Config{..} <- loadConfig 
    Server.Env{..} <- Server.loadEnv @s
    let fullAddress = "http://"
                   <> T.unpack cServerAddress
                   <> "/relayRequestSubmitTx"
    nakedRequest <- parseRequest fullAddress
    manager <- newManager defaultManagerSettings
    let mkRequest' = mkRequest @s nakedRequest manager
    runClientM envAuxiliary envWallet $ withGreetings $ case opts of
        Manual cReq          -> mkRequest' cReq
        Auto AutoOptions{..} -> forever $ do
            cReq <- genRequest @s maxTokensInReq
            mkRequest' cReq
            waitTime =<< randomRIO (1, averageRequestInterval * 2)
    where
        withGreetings = (logMsg "Starting client..." >>)

mkRequest :: forall s. HasClient s => Request -> Manager -> ClientRequestOf s -> ClientM s ()
mkRequest nakedReq manager clientReq = do
        logMsg $ "New tokens to send:\n" .< clientReq
        (onSuccess, red) <- makeServerInput clientReq
        let req = nakedReq
                { method = "POST"
                , requestBody = RequestBodyLBS $ encode red
                , requestHeaders = [(hContentType, "application/json")]
                , responseTimeout = responseTimeoutMicro (3 * 60 * 1_000_000)
                }
        resp <- liftIO $ httpLbs req manager
        logMsg $ "Received response:" .< resp
        when (successful resp) onSuccess
    where
        successful = (== status204) . responseStatus

genRequest :: forall s m. HasClient s => MonadIO m => Maximum -> m (ClientRequestOf s)
genRequest ub = liftIO $ do
    len <- randomRIO (1, ub)
    nub <$> replicateM len (genRequestTerm @s)