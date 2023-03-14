{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}

module Cardano.Server.Class where

import           Cardano.Node.Emulator             (Params)
import           Cardano.Server.Config             (InactiveEndpoints, decodeOrErrorFromFile)
import           Cardano.Server.Error.CommonErrors (InternalServerError (NoWalletProvided))
import           Cardano.Server.Input              (InputContext)
import           Cardano.Server.Utils.Logger       (HasLogger)
import           Control.Exception                 (throw)
import           Control.Monad.Catch               (MonadCatch, MonadThrow)
import           Control.Monad.IO.Class            (MonadIO)
import           Control.Monad.Reader              (MonadReader, ReaderT, asks)
import           Data.Aeson                        (FromJSON (..), ToJSON)
import           Data.Data                         (Typeable)
import           Data.Functor                      ((<&>))
import           Data.IORef                        (IORef)
import           Data.Kind                         (Type)
import           Data.Maybe                        (fromMaybe)
import           Data.Sequence                     (Seq)
import           Ledger                            (TxOutRef)
import           Ledger.Address                    (Address)
import           PlutusAppsExtra.IO.Blockfrost     (BfToken)
import           PlutusAppsExtra.IO.ChainIndex     (ChainIndex (..), HasChainIndex (..))
import           PlutusAppsExtra.IO.Wallet         (HasWallet (..), RestoredWallet, getWalletAddr)
import           Servant                           (JSON, MimeUnrender)
import PlutusAppsExtra.Types.Tx (TransactionBuilder)

-- class ( Show (AuxiliaryEnvOf s)
--       , MimeUnrender JSON (InputOf s)
--       , ToJSON (InputOf s)
--       , Show (InputOf s)
--       , Typeable s
--       ) => HasServer s where

--     type AuxiliaryEnvOf s :: Type

--     loadAuxiliaryEnv :: FilePath -> IO (AuxiliaryEnvOf s)
--     default loadAuxiliaryEnv :: FromJSON (AuxiliaryEnvOf s) => FilePath -> IO (AuxiliaryEnvOf s)
--     loadAuxiliaryEnv = decodeOrErrorFromFile

--     type InputOf s :: Type

--     serverSetup :: (MonadCatch m, MonadReader (Env s) m, HasLogger m, HasWallet m, HasChainIndex m) => m ()
--     serverSetup = pure ()

--     serverIdle :: (MonadCatch m, MonadReader (Env s) m, HasLogger m, HasWallet m, HasChainIndex m) => m ()
--     serverIdle = pure ()

--     serverTrackedAddresses :: (MonadReader (Env s) m, HasWallet m) => m [Address]
--     serverTrackedAddresses = (:[]) <$> getWalletAddr

--     -- This chainindex will be used when no other is specified in the config
--     defaultChainIndex :: ChainIndex
--     defaultChainIndex = Plutus

