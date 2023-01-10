{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

module Client.Client where

import           Client.Class              (HasClient(..))
import           Client.Opts               (Options(..), runWithOpts, Mode (..))
import           Control.Monad.Reader      (MonadIO(..), forever, when)
import           Data.Aeson                (encode, ToJSON)
import           Data.ByteString.Lazy      (ByteString)
import qualified Data.Text                 as T
import           Network.HTTP.Client       (httpLbs, defaultManagerSettings, newManager, parseRequest,
                                            Manager, Request(..), RequestBody(..), responseStatus, responseTimeoutMicro, Response)
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
    manager <- newManager defaultManagerSettings
    let client' = client @s fullAddress manager
    runAppM $ withGreetings $ case optsMode of
        Manual serverInput          -> client' serverInput
        Auto   averageInterval -> forever $ do
            serverInput <- genServerInput @s
            client' serverInput
            waitTime =<< randomRIO (1, averageInterval * 2)
    where
        withGreetings = (logMsg "Starting client..." >>)

client :: forall s. HasClient s => String -> Manager -> InputOf s -> AppM s ()
client fullAddress manager serverInput = do
        (beforeRequestSend, onSuccessfulResponse) <- extractActionsFromInput serverInput
        beforeRequestSend
        resp <- addExternalUtxosToInput serverInput >>= mkRequest fullAddress manager 
        when (successful resp) onSuccessfulResponse
    where
        successful = (== status204) . responseStatus

mkRequest :: (ToJSON body, Show body) => String -> Manager -> body -> AppM s (Response ByteString)
mkRequest fullAddress manager body = do
    logMsg $ "New request to send:\n" .< body
    nakedRequest <- liftIO $ parseRequest fullAddress
    let req = nakedRequest
            { method = "POST"
            , requestBody = RequestBodyLBS $ encode body
            , requestHeaders = [(hContentType, "application/json")]
            , responseTimeout = responseTimeoutMicro (3 * 60 * 1_000_000)
            }
    resp <- liftIO $ httpLbs req manager
    logMsg $ "Received response:" .< resp
    pure resp