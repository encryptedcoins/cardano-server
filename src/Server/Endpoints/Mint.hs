{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}

module Server.Endpoints.Mint where

import           Control.Monad                    (void, when)
import           Control.Monad.Catch              (Exception, SomeException, catch, handle, MonadThrow, MonadCatch)
import           Control.Monad.IO.Class           (MonadIO(..))
import           Control.Monad.Reader             (ReaderT(..), MonadReader, asks)
import           Data.Kind                        (Type)
import           Data.IORef                       (atomicWriteIORef, atomicModifyIORef, readIORef)
import           Data.Sequence                    (Seq(..), (|>))
import           IO.ChainIndex                    (getUtxosAt)
import           IO.Wallet                        (HasWallet(..), getWalletAddr)
import           Servant                          (NoContent(..), JSON, (:>), ReqBody, respond, StdMethod(POST), UVerb, Union, IsMember)
import           Server.Internal                  (getQueueRef, AppM, Env(..), HasServer(..), QueueRef)
import           Server.Tx                        (mkWalletTxOutRefs)
import           Utils.ChainIndex                 (filterCleanUtxos)
import           Utils.Logger                     (HasLogger(..), (.<), logSmth)
import           Utils.Wait                       (waitTime)

type MintApi s = "relayRequestMint"
              :> ReqBody '[JSON] (RedeemerOf s)
              :> UVerb 'POST '[JSON] (MintApiResultOf s)

mintHandler :: forall s. HasMintEndpoint s => RedeemerOf s -> AppM s (Union (MintApiResultOf s))
mintHandler red = handle mintErrorHanlder $ do
        logMsg $ "New mint request received:\n" .< red
        checkForErrors
        ref <- getQueueRef
        liftIO $ atomicModifyIORef ref ((,()) . (|> red))
        respond NoContent
    where
        checkForErrors = checkForMintErros red >> checkForCleanUtxos
        checkForCleanUtxos = do
            addr       <- getWalletAddr
            cleanUtxos <- length . filterCleanUtxos <$> liftIO (getUtxosAt addr)
            minUtxos   <- asks envMinUtxosAmount
            when (cleanUtxos < minUtxos) $ do
                logMsg "Address doesn't has enough clean UTXO's."
                void $ mkWalletTxOutRefs addr (cleanUtxos - minUtxos)

class ( HasServer s
      , IsMember NoContent (MintApiResultOf s)
      , Show (MintErrorOf s)
      , Exception (MintErrorOf s)
      ) => HasMintEndpoint s where

    type MintApiResultOf s :: [Type]

    data MintErrorOf s

    checkForMintErros :: RedeemerOf s -> AppM s ()

    mintErrorHanlder :: MintErrorOf s -> AppM s (Union (MintApiResultOf s))

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

processQueue :: forall s. HasServer s => Env s -> IO ()
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
                red :<| reds -> processRedeemer qRef red reds >> go
        logIdle n = when (n `mod` 100 == 0) $ logMsg "No new redeemers to process."

processRedeemer :: HasServer s => QueueRef s -> RedeemerOf s -> Seq (RedeemerOf s) -> QueueM s ()
processRedeemer qRef red reds = do
    liftIO $ atomicWriteIORef qRef reds
    logMsg $ "New redeemer to process:" .< red
    processTokens red