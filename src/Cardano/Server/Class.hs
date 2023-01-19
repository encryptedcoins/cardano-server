{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeFamilies               #-}

module Cardano.Server.Class where

import           Cardano.Server.Config  (decodeOrErrorFromFile)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (ReaderT, MonadReader, asks)
import           Data.Aeson             (FromJSON(..), ToJSON)
import           Data.IORef             (IORef)
import           Data.Kind              (Type)
import           Data.Sequence          (Seq)
import           IO.Wallet              (HasWallet(..), RestoredWallet, getWalletAddr)
import           Ledger                 (Params)
import           Ledger.Address         (Address)
import           Servant                (MimeUnrender, JSON)
import           Utils.ChainIndex       (MapUTXO)
import Data.Data (Typeable)

class ( Show (AuxiliaryEnvOf s)
      , MimeUnrender JSON (InputOf s)
      , ToJSON (InputOf s)
      , Show (InputOf s)
      , Typeable s
      ) => HasServer s where

    type AuxiliaryEnvOf s :: Type

    loadAuxiliaryEnv :: FilePath -> IO (AuxiliaryEnvOf s)
    default loadAuxiliaryEnv :: FromJSON (AuxiliaryEnvOf s) => FilePath -> IO (AuxiliaryEnvOf s)
    loadAuxiliaryEnv = decodeOrErrorFromFile

    type InputOf s :: Type

    serverSetup :: (MonadIO m, MonadReader (Env s) m) => m ()
    serverSetup = pure ()

    serverIdle :: (MonadIO m, MonadReader (Env s) m) => m ()
    serverIdle = pure ()

    serverTrackedAddresses :: (MonadReader (Env s) m, HasWallet m) => m [Address]
    serverTrackedAddresses = (:[]) <$> getWalletAddr

type QueueElem s = (InputOf s, MapUTXO)

type Queue s = Seq (QueueElem s)

type QueueRef s = IORef (Queue s)

data Env s = Env
    { envQueueRef           :: QueueRef s
    , envWallet             :: RestoredWallet
    , envAuxiliary          :: AuxiliaryEnvOf s
    , envMinUtxosAmount     :: Int
    , envLedgerParams       :: Params
    }

instance MonadIO m => HasWallet (ReaderT (Env s) m) where 
    getRestoredWallet = asks envWallet