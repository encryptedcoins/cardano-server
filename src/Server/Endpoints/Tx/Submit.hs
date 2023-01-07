{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TupleSections              #-}

module Server.Endpoints.Tx.Submit where

import           Control.Monad                    (join, void, when, liftM3)
import           Control.Monad.IO.Class           (MonadIO(..))
import           Control.Monad.Catch              (SomeException, catch, handle, MonadThrow, MonadCatch)
import           Control.Monad.Reader             (ReaderT(..), MonadReader, asks)
import           Data.IORef                       (atomicWriteIORef, atomicModifyIORef, readIORef)
import           Data.Sequence                    (Seq(..), (|>))
import           IO.Wallet                        (HasWallet(..))
import           Servant                          (NoContent(..), JSON, (:>), ReqBody, respond, StdMethod(POST), UVerb, Union)
import           Server.Endpoints.Tx.Internal     (HasTxEndpoints(..))     
import           Server.Internal                  (getQueueRef, AppM, Env(..), HasServer(..), QueueRef, checkForCleanUtxos, QueueElem, Queue)
import           Server.Tx                        (mkTx)
import           Utils.ChainIndex                 (MapUTXO)
import           Utils.Logger                     (HasLogger(..), (.<), logSmth)
import           Utils.Wait                       (waitTime)

type SubmitTxApi s = "relayRequestSubmitTx"
              :> ReqBody '[JSON] (RedeemerOf s, MapUTXO)
              :> UVerb 'POST '[JSON] (TxApiResultOf s)

submitTxHandler :: forall s. HasTxEndpoints s => (RedeemerOf s, MapUTXO) ->  AppM s (Union (TxApiResultOf s))
submitTxHandler arg@(red, utxosExternal) = handle txEndpointsErrorHanlder $ do
    logMsg $ "New submitTx request received:" 
        <> "\nRedeemer:" .< red
        <> "\nUtxos:"    .< utxosExternal
    checkForTxEndpointsErros red
    ref <- getQueueRef
    liftIO $ atomicModifyIORef ref ((,()) . (|> arg))
    respond NoContent

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
                red :<| reds -> processQueueElem qRef red reds >> go
        logIdle n = when (n `mod` 100 == 0) $ logMsg "No new redeemers to process."

processQueueElem :: forall s. HasTxEndpoints s => QueueRef s -> QueueElem s -> Queue s -> QueueM s ()
processQueueElem qRef qElem@(red, externalUtxos) elems = do
    liftIO $ atomicWriteIORef qRef elems
    logMsg $ "New redeemer to process:" .< red <> "\nUtxos:" .< externalUtxos
    processTokens @s qElem

processTokens :: forall s m. (HasTxEndpoints s, HasWallet m, HasLogger m, MonadReader (Env s) m) => QueueElem s -> m ()
processTokens (red, utxosExternal) = do
    checkForCleanUtxos
    void $ join $ liftM3 mkTx (getTrackedAddresses @s) (pure utxosExternal) $ txEndpointsTxBuilders @s red