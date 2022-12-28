{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}

module Server.Endpoints.Tx.Internal where

import           Control.Monad.Catch              (Exception)
import           Control.Monad.Reader             (MonadReader)
import           Control.Monad.State              (State)
import           Data.Aeson                       (ToJSON)
import           Data.Kind                        (Type)
import           Data.Text                        (Text)
import           GHC.Generics                     (Generic)
import           IO.Wallet                        (HasWallet(..), getWalletAddr)
import           Ledger                           (Address)
import           Plutus.Script.Utils.Typed        (Any, ValidatorTypes(..))
import           Servant                          (NoContent(..), Union, IsMember, WithStatus, HasStatus)
import           Server.Internal                  (AppM, HasServer(..), Env)
import           Types.Tx                         (TxConstructor)

class ( HasServer s
      , IsMember NoContent             (TxApiResultOf s)
      , IsMember NewTxEndpointResult   (TxApiResultOf s)
      , IsMember (WithStatus 422 Text) (TxApiResultOf s)
      , Show (TxEndpointsErrorOf s)
      , Exception (TxEndpointsErrorOf s)
      ) => HasTxEndpoints s where

    type TxApiResultOf s :: [Type]

    data TxEndpointsErrorOf s

    checkForTxEndpointsErros :: RedeemerOf s -> AppM s ()

    txEndpointsErrorHanlder :: TxEndpointsErrorOf s -> AppM s (Union (TxApiResultOf s))

    getTrackedAddresses :: (MonadReader (Env s) m, HasWallet m) => m [Address]
    getTrackedAddresses = (:[]) <$> getWalletAddr

    txEndpointsTxBuilders :: (MonadReader (Env s) m, HasWallet m) => RedeemerOf s -> m [State (TxConstructor Any (RedeemerType Any) (DatumType Any)) ()]

type DefaultTxApiResult = '[WithStatus 422 Text, NoContent, NewTxEndpointResult]

newtype NewTxEndpointResult = NewTxEndpointResult Text
    deriving HasStatus via WithStatus 200 NewTxEndpointResult
    deriving (Show, Generic)
    deriving newtype ToJSON 