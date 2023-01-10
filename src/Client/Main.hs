{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

module Client.Main where

import           Client.Class              (HasClient(..))
import           Client.Opts               (Options(..), runWithOpts, Mode (..))
import           Control.Monad.Reader      (MonadIO(..), forever, when)
import           Data.Aeson                (encode)
import qualified Data.Text                 as T
import           Network.HTTP.Client       (httpLbs, defaultManagerSettings, newManager, parseRequest,
                                            Manager, Request(..), RequestBody(..), responseStatus, responseTimeoutMicro)
import           Network.HTTP.Types.Header (hContentType)
import           Network.HTTP.Types.Status (status204)
import           Server.Internal           (AppM, runAppM, HasServer(..))
import           Server.Config             (Config(..), loadConfig)       
import           System.Random             (randomRIO)
import           Utils.Logger              (HasLogger(..), (.<))
import           Utils.Wait                (waitTime)

startClient :: forall s. HasClient s => IO ()
startClient = do
    Options{..} <- runWithOpts @s
    Config{..} <- loadConfig 
    let fullAddress = concat 
            ["http://", T.unpack cServerAddress, "/relayRequest", show optsEndpoint]
    nakedRequest <- parseRequest fullAddress
    manager <- newManager defaultManagerSettings
    let mkRequest' = mkRequest @s nakedRequest manager
    runAppM $ withGreetings $ case optsMode of
        Manual serverInput          -> mkRequest' serverInput
        Auto   averageInterval -> forever $ do
            serverInput <- genServerInput @s
            mkRequest' serverInput
            waitTime =<< randomRIO (1, averageInterval * 2)
    where
        withGreetings = (logMsg "Starting client..." >>)

mkRequest :: forall s. HasClient s => Request -> Manager -> InputOf s -> AppM s ()
mkRequest nakedReq manager serverInput = do
        logMsg $ "New input to send:\n" .< serverInput
        (beforeRequestSend, onSuccessfulResponse) <- extractActionsFromInput serverInput
        let req = nakedReq
                { method = "POST"
                , requestBody = RequestBodyLBS $ encode serverInput
                , requestHeaders = [(hContentType, "application/json")]
                , responseTimeout = responseTimeoutMicro (3 * 60 * 1_000_000)
                }
        beforeRequestSend
        resp <- liftIO $ httpLbs req manager
        logMsg $ "Received response:" .< resp
        when (successful resp) onSuccessfulResponse
    where
        successful = (== status204) . responseStatus