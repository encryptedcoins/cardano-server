{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

module Server.Internal 
    ( module Server.Class
    , NetworkM(..)
    , AppM(..)
    , runAppM
    , getQueueRef
    , loadEnv
    , checkForCleanUtxos
    ) where

import           Control.Monad          (void, when)
import           Control.Monad.Catch    (MonadThrow, MonadCatch)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (ReaderT(ReaderT, runReaderT), MonadReader, asks)
import           Data.IORef             (newIORef)
import           Data.Sequence          (empty)
import           IO.ChainIndex          (getWalletUtxos)
import           IO.Wallet              (HasWallet(..), getWalletAddr)
import           Servant                (Handler)
import           Server.Class           (Env(..), Queue, QueueRef, QueueElem, HasServer(..))
import           Server.Config          (Config(..), configFile, decodeOrErrorFromFile)
import           Server.Tx              (mkWalletTxOutRefs)
import           Utils.ChainIndex       (filterCleanUtxos)
import           Utils.Logger           (HasLogger(..))

newtype NetworkM s a = NetworkM { unNetworkM :: ReaderT (Env s) Handler a }
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

instance HasLogger (NetworkM s) where
    loggerFilePath = "server.log"

getQueueRef :: NetworkM s (QueueRef s)
getQueueRef = asks envQueueRef

loadEnv :: forall s. HasServer s => IO (Env s)
loadEnv = do
    Config{..} <- decodeOrErrorFromFile configFile
    let envMinUtxosAmount = cMinUtxosAmount
    envQueueRef  <- newIORef empty
    envWallet    <- decodeOrErrorFromFile cWalletFile
    envAuxiliary <- loadAuxiliaryEnv @s cAuxiliaryEnvFile
    pure Env{..}

newtype AppM s a = AppM { unAppM :: ReaderT (Env s) IO a }
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader (Env s))

runAppM :: HasServer s => AppM s a -> IO a
runAppM app = loadEnv >>= runReaderT (unAppM app)

instance HasLogger (AppM s) where
    loggerFilePath = "server.log"

instance HasWallet (AppM s) where
    getRestoredWallet = asks envWallet

checkForCleanUtxos :: (HasWallet m, HasLogger m, MonadReader (Env s) m) => m ()
checkForCleanUtxos = do
    addr       <- getWalletAddr
    cleanUtxos <- length . filterCleanUtxos <$> getWalletUtxos
    minUtxos   <- asks envMinUtxosAmount
    when (cleanUtxos < minUtxos) $ do
        logMsg "Address doesn't has enough clean UTXO's."
        void $ mkWalletTxOutRefs addr (cleanUtxos - minUtxos)