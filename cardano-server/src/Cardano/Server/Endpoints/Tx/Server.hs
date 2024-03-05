{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Cardano.Server.Endpoints.Tx.Server where

import           Cardano.Server.Config                (ServerEndpoint (ServerTxE))
import           Cardano.Server.Endpoints.Tx.Internal (TxApiErrorOf)
import           Cardano.Server.Error                 (ConnectionError, Envelope, IsCardanoServerError, Throws, toEnvelope)
import           Cardano.Server.Internal              (Env (envQueueRef), InputOf, Queue, QueueElem (..), QueueRef, ServerM,
                                                       TxApiRequestOf, checkEndpointAvailability, getQueueRef, newQueueElem,
                                                       runServerM, serverIdle, serverTrackedAddresses, setLoggerFilePath,
                                                       txEndpointProcessRequest, txEndpointsTxBuilders)
import           Cardano.Server.Tx                    (checkForCleanUtxos, submitTx)
import           Cardano.Server.Utils.Logger          (logMsg, logSmth, (.<))
import           Cardano.Server.Utils.Wait            (waitTime)
import           Control.Concurrent                   (putMVar, takeMVar)
import           Control.Monad                        (join, liftM3, when)
import           Control.Monad.Catch                  (SomeException, catch, fromException, handle, throwM)
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

serverTxHandler :: forall api. (Show (TxApiRequestOf api), IsCardanoServerError (TxApiErrorOf api))
    => TxApiRequestOf api
    -> ServerM api (Envelope '[TxApiErrorOf api, ConnectionError] NoContent)
serverTxHandler req = toEnvelope $ do
        logMsg $ "New serverTx request received:\n" .< req
        checkEndpointAvailability ServerTxE
        qElem <- txEndpointProcessRequest req >>= newQueueElem
        ref   <- getQueueRef
        liftIO $ atomicModifyIORef ref ((,()) . (|> qElem))
        liftIO (takeMVar $ qeMvar qElem) >>= either reThrow (const $ return NoContent)
    where
        reThrow (fromException @(TxApiErrorOf api) -> Just txApiErr)  = throwM txApiErr
        reThrow (fromException @ConnectionError    -> Just connError) = throwM connError
        reThrow err                                                   = throwM err

processQueue ::  (Show (InputOf api)) => Env api -> IO ()
processQueue env = runServerM env $ setLoggerFilePath "queue.log" $ do
        logMsg "Starting queue handler..."
        neverFall go
    where
        neverFall ma = catch ma $ \(err :: SomeException) -> do
            logSmth err
            waitTime 3
            neverFall ma
        go = liftIO getCurrentTime >>= checkQueue
        checkQueue t = do
            qRef <- asks envQueueRef
            liftIO (readIORef qRef) >>= \case
                Empty -> idleQueue t >>= checkQueue
                e :<| es -> processQueueElem qRef e es >> go

idleQueue :: Time.UTCTime -> ServerM api Time.UTCTime
idleQueue st = do
    ct <- liftIO getCurrentTime
    let delta = Time.diffUTCTime ct st
        enoughTimePassed = delta > 300
        firstTime        = delta < 3
    when (enoughTimePassed || firstTime) $ logMsg "No new inputs to process."
    checkForCleanUtxos
    serverIdle
    pure $ if enoughTimePassed then ct else st

processQueueElem :: (Show (InputOf api)) => QueueRef api -> QueueElem api -> Queue api -> ServerM api ()
processQueueElem qRef QueueElem{..} elems = handle h $ do
        liftIO $ atomicWriteIORef qRef elems
        logMsg $ "New input to process:" .< qeInput <> "\nContext:" .< qeContext
        checkForCleanUtxos
        join $ liftM3 submitTx serverTrackedAddresses (pure qeContext) $ txEndpointsTxBuilders qeInput
        logMsg "Submited."
        liftIO $ putMVar qeMvar $ Right ()
    where
        h = liftIO . putMVar qeMvar . Left