{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

module Cardano.Server.Endpoints.Tx.Server where

import           Cardano.Server.Class              (InputWithContext)
import           Cardano.Server.Config             (isInactiveServerTx)
import           Cardano.Server.Endpoints.Tx.Class (HasTxEndpoints (..))
import           Cardano.Server.Error              (ConnectionError, Envelope, Throws, toEnvelope)
import           Cardano.Server.Internal           (Env (..), HasServer (..), NetworkM, Queue, QueueRef,
                                                    checkEndpointAvailability, getQueueRef)
import           Cardano.Server.Tx                 (MkTxConstraints, checkForCleanUtxos, mkTx)
import           Cardano.Server.Utils.Logger       (HasLogger (..), logSmth, (.<))
import           Cardano.Server.Utils.Wait         (waitTime)
import           Control.Monad                     (join, liftM3, void, when)
import           Control.Monad.Catch               (MonadCatch, MonadThrow, SomeException, catch)
import           Control.Monad.IO.Class            (MonadIO (..))
import           Control.Monad.Reader              (MonadReader, ReaderT (..), asks)
import           Data.IORef                        (atomicModifyIORef, atomicWriteIORef, readIORef)
import           Data.Sequence                     (Seq (..), (|>))
import           PlutusAppsExtra.IO.Wallet         (HasWallet (..))
import           Servant                           (JSON, NoContent (..), Post, ReqBody, (:>))

type ServerTxApi s = "serverTx"
    :> Throws ConnectionError
    :> ReqBody '[JSON] (TxApiRequestOf s)
    :> Post '[JSON] NoContent

serverTxHandler :: forall s. HasTxEndpoints s
    => TxApiRequestOf s
    -> NetworkM s (Envelope '[ConnectionError] NoContent)
serverTxHandler req = toEnvelope $ do
    logMsg $ "New serverTx request received:\n" .< req
    checkEndpointAvailability isInactiveServerTx
    arg <- txEndpointsProcessRequest req
    ref <- getQueueRef
    liftIO $ atomicModifyIORef ref ((,()) . (|> arg))
    pure NoContent

newtype QueueM s a = QueueM { unQueueM :: ReaderT (Env s) IO a }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader (Env s)
        , MonadThrow
        , MonadCatch
        , HasWallet
        )

instance HasLogger (QueueM s) where
    loggerFilePath = "queue.log"

runQueueM :: Env s -> QueueM s () -> IO ()
runQueueM env = flip runReaderT env . unQueueM

processQueue :: forall s. HasTxEndpoints s => Env s -> IO ()
processQueue env = runQueueM env $ do
        logMsg "Starting queue handler..."
        catch go $ \(err :: SomeException) -> do
            logSmth err
            go
    where
        go = checkQueue (0 :: Int)
        checkQueue n = do
            qRef <- asks envQueueRef
            liftIO (readIORef qRef) >>= \case
                Empty -> logIdle n >> waitTime 3 >> checkQueue (n + 1)
                input :<| inputs -> processQueueElem qRef input inputs >> go
        logIdle n = when (n `mod` 100 == 0) $ logMsg "No new inputs to process."

processQueueElem :: forall s. HasTxEndpoints s => QueueRef s -> InputWithContext s -> Queue s -> QueueM s ()
processQueueElem qRef qElem@(input, context) elems = do
    liftIO $ atomicWriteIORef qRef elems
    logMsg $ "New input to process:" .< input <> "\nContext:" .< context
    processInputs @s qElem

processInputs :: forall s m.
    ( HasTxEndpoints s
    , MkTxConstraints m s
    ) => InputWithContext s -> m ()
processInputs (input, context) = do
    checkForCleanUtxos
    void $ join $ liftM3 mkTx (serverTrackedAddresses @s) (pure context) $ txEndpointsTxBuilders @s input