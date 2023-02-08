{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE LambdaCase     #-}

module Cardano.Server.Utils.ChainIndex where

import           Control.Monad.IO.Class           (MonadIO (..))
import           Data.Aeson                       (FromJSON)
import           GHC.Generics                     (Generic)
import           Ledger                           (Address)
import qualified PlutusAppsExtra.IO.ChainIndex    as Plutus
import qualified PlutusAppsExtra.IO.Kupo          as Kupo
import           PlutusAppsExtra.Utils.ChainIndex (MapUTXO)

data ChainIndex = Plutus | Kupo
    deriving (Show, Generic, FromJSON)

class MonadIO m => HasChainIndex m where
    getChainIndex :: m ChainIndex

getUtxosAt :: HasChainIndex m => Address -> m MapUTXO
getUtxosAt addr = getChainIndex >>= \case
    Plutus -> liftIO $ Plutus.getUtxosAt addr
    Kupo   -> liftIO $   Kupo.getUtxosAt addr