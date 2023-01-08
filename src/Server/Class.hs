{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DefaultSignatures #-}

module Server.Class where

import           Control.Monad          (void, when)
import           Control.Monad.Catch    (MonadThrow, MonadCatch)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (ReaderT(ReaderT, runReaderT), MonadReader, asks)
import           Data.Aeson             (FromJSON(..), ToJSON)
import           Data.IORef             (IORef, newIORef)
import           Data.Kind              (Type)
import           Data.Sequence          (Seq, empty)
import           IO.ChainIndex          (getWalletUtxos)
import           IO.Wallet              (HasWallet(..), RestoredWallet, getWalletAddr)
import           Ledger.Address         (Address)
import           Servant                (Handler, MimeUnrender, JSON)
import           Server.Config          (Config(..), configFile, decodeOrErrorFromFile)
import           Server.Tx              (mkWalletTxOutRefs)
import           Utils.ChainIndex       (filterCleanUtxos, MapUTXO)
import           Utils.Logger           (HasLogger(..))

class ( Show (AuxiliaryEnvOf s)
      , MimeUnrender JSON (InputOf s)
      , ToJSON (InputOf s)
      , Show (InputOf s)
      ) => HasServer s where

    type AuxiliaryEnvOf s :: Type

    loadAuxiliaryEnv :: FilePath -> IO (AuxiliaryEnvOf s)
    default loadAuxiliaryEnv :: FromJSON (AuxiliaryEnvOf s) => FilePath -> IO (AuxiliaryEnvOf s)
    loadAuxiliaryEnv = decodeOrErrorFromFile

    type InputOf s :: Type

    serverSetup :: AppM s ()
    serverSetup = pure ()

    serverIdle :: AppM s ()
    serverIdle = pure ()

    serverTrackedAddresses :: (MonadReader (Env s) m, HasWallet m) => m [Address]
    serverTrackedAddresses = (:[]) <$> getWalletAddr

newtype AppM s a = AppM { unAppM :: ReaderT (Env s) Handler a }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader (Env s)
        , HasWallet
        , MonadThrow
        , MonadCatch
        )

instance HasLogger (AppM s) where
    loggerFilePath = "server.log"

instance (Monad m, MonadIO m) => HasWallet (ReaderT (Env s) m) where
    getRestoredWallet = asks envWallet

type QueueElem s = (InputOf s, MapUTXO)

type Queue s = Seq (QueueElem s)

type QueueRef s = IORef (Queue s)

data Env s = Env
    { envQueueRef       :: QueueRef s
    , envWallet         :: RestoredWallet
    , envAuxiliary      :: AuxiliaryEnvOf s
    , envMinUtxosAmount :: Int
    }

getQueueRef :: AppM s (QueueRef s)
getQueueRef = asks envQueueRef

loadEnv :: forall s. HasServer s => IO (Env s)
loadEnv = do
    Config{..} <- decodeOrErrorFromFile configFile
    let envMinUtxosAmount = cMinUtxosAmount
    envQueueRef  <- newIORef empty
    envWallet    <- decodeOrErrorFromFile cWalletFile
    envAuxiliary <- loadAuxiliaryEnv @s cAuxiliaryEnvFile
    pure Env{..}

newtype SetupM s a = SetupM { unSetupM :: ReaderT (Env s) IO a }
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader (Env s))

runSetupM :: Env s -> SetupM s a -> IO a
runSetupM env = (`runReaderT` env) . unSetupM

instance HasLogger (SetupM s) where
    loggerFilePath = "server.log"

instance HasWallet (SetupM s) where
    getRestoredWallet = asks envWallet

checkForCleanUtxos :: (HasWallet m, HasLogger m, MonadReader (Env s) m) => m ()
checkForCleanUtxos = do
    addr       <- getWalletAddr
    cleanUtxos <- length . filterCleanUtxos <$> getWalletUtxos
    minUtxos   <- asks envMinUtxosAmount
    when (cleanUtxos < minUtxos) $ do
        logMsg "Address doesn't has enough clean UTXO's."
        void $ mkWalletTxOutRefs addr (cleanUtxos - minUtxos)