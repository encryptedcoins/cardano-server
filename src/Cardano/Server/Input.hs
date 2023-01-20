{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeFamilies               #-}

module Cardano.Server.Input where

import           Data.Aeson             (ToJSON, FromJSON)
import           Data.Default           (Default (..))
import           GHC.Generics           (Generic)
import           Ledger                 (TxOutRef)
import           Ledger.Address         (Address)
import           Utils.ChainIndex       (MapUTXO)

data WalletType = ServerWallet | UserWallet
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

data InputContext = InputContext
    {
        inputWalletType    :: WalletType,       -- Determines how do we balance a transaction
        inputUTXO          :: MapUTXO,          -- Unspent outputs for satisfying transaction constraints
        inputWalletUTXO    :: MapUTXO,          -- Unspent outputs for balancing an unbalanced transaction
        inputCollateral    :: Maybe TxOutRef,   -- Reference of a collateral input
        inputChangeAddress :: Maybe Address     -- Address for the change
    }
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance Default InputContext where
    def = InputContext ServerWallet mempty mempty Nothing Nothing