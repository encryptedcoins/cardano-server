{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Cardano.Server.Endpoints.Tx.Server where

import           Cardano.Server.Config                (isInactiveServerTx)
import           Cardano.Server.Endpoints.Tx.Internal (TxApiErrorOf)
import           Cardano.Server.Error                 (ConnectionError, Envelope, IsCardanoServerError, Throws, toEnvelope)
import           Cardano.Server.Internal              (Env (envQueueRef), InputOf, InputWithContext, Queue, QueueRef, ServerM,
                                                       TxApiRequestOf, checkEndpointAvailability, getQueueRef, runServerM,
                                                       serverIdle, serverTrackedAddresses, setLoggerFilePath,
                                                       txEndpointProcessRequest, txEndpointsTxBuilders)
import           Cardano.Server.Tx                    (checkForCleanUtxos, mkTx)
import           Cardano.Server.Utils.Logger          (logMsg, logSmth, (.<))
import           Cardano.Server.Utils.Wait            (waitTime)
import           Control.Monad                        (join, liftM3, void, when)
import           Control.Monad.Catch                  (SomeException, catch)
import           Control.Monad.IO.Class               (MonadIO (..))
import           Control.Monad.Reader                 (asks)
import           Data.IORef                           (atomicModifyIORef, atomicWriteIORef, readIORef)
import           Data.Sequence                        (Seq (..), (|>))
import           Data.Time                            (getCurrentTime)
import qualified Data.Time                            as Time
import           Servant                              (JSON, NoContent (..), Post, ReqBody, (:>))

type ServerTxApi reqBody err = "serverTx"
    :> Throws err
    :> Throws ConnectionError
    :> ReqBody '[JSON] reqBody
    :> Post '[JSON] NoContent

serverTxHandler :: (Show (TxApiRequestOf api), IsCardanoServerError (TxApiErrorOf api))
    => TxApiRequestOf api
    -> ServerM api (Envelope '[TxApiErrorOf api, ConnectionError] NoContent)
serverTxHandler req = toEnvelope $ do
    logMsg $ "New serverTx request received:\n" .< req
    checkEndpointAvailability isInactiveServerTx
    arg <- txEndpointProcessRequest req
    ref <- getQueueRef
    liftIO $ atomicModifyIORef ref ((,()) . (|> arg))
    pure NoContent

processQueue ::  (Show (InputOf api)) => Env api -> IO ()
processQueue env = runServerM env $ setLoggerFilePath "queue.log" $ do
        logMsg "Starting queue handler..."
        neverFall go
    where
        neverFall ma = catch ma $ \(err :: SomeException) -> do
            logSmth err
            neverFall ma
        go = liftIO getCurrentTime >>= checkQueue
        checkQueue t = do
            qRef <- asks envQueueRef
            liftIO (readIORef qRef) >>= \case
                Empty -> idleQueue t >>= checkQueue
                input :<| inputs -> processQueueElem qRef input inputs >> go

idleQueue :: Time.UTCTime -> ServerM api Time.UTCTime
idleQueue st = do
    ct <- liftIO getCurrentTime
    let delta = Time.diffUTCTime ct st
        enoughTimePassed = delta > 300
        firstTime        = delta < 3
    when (enoughTimePassed || firstTime) $ logMsg "No new inputs to process."
    checkForCleanUtxos
    serverIdle
    waitTime 3
    pure $ if enoughTimePassed then ct else st

processQueueElem :: (Show (InputOf api)) => QueueRef api -> InputWithContext api -> Queue api -> ServerM api ()
processQueueElem qRef qElem@(input, context) elems = do
    liftIO $ atomicWriteIORef qRef elems
    logMsg $ "New input to process:" .< input <> "\nContext:" .< context
    processInputs qElem

processInputs :: InputWithContext api -> ServerM api ()
processInputs (input, context) = do
    checkForCleanUtxos
    void $ join $ liftM3 mkTx serverTrackedAddresses (pure context) $ txEndpointsTxBuilders input