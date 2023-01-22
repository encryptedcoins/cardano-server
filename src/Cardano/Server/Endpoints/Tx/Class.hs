{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Cardano.Server.Endpoints.Tx.Class where

import           Cardano.Server.Class                 (HasServer(..), Env, InputWithContext)
import           Cardano.Server.Error                 (IsCardanoServerError, ExceptionDeriving(..))
import           Cardano.Server.Internal              (NetworkM, )
import           Control.Exception                    (Exception)
import           Control.Monad.Reader                 (MonadReader)
import           Data.Kind                            (Type)
import           IO.Wallet                            (HasWallet(..))
import           Types.Tx                             (TransactionBuilder)

class ( HasServer s
      , Show (TxApiRequestOf s)
      , Show (TxEndpointsErrorOf s)
      , IsCardanoServerError (TxEndpointsErrorOf s)
      ) => HasTxEndpoints s where

    type TxApiRequestOf s :: Type

    data TxEndpointsErrorOf s

    txEndpointsProcessRequest :: TxApiRequestOf s -> NetworkM s (InputWithContext s)

    txEndpointsTxBuilders     :: (MonadReader (Env s) m, HasWallet m) => InputOf s -> m [TransactionBuilder ()]

deriving via (ExceptionDeriving (TxEndpointsErrorOf s)) instance HasTxEndpoints s => Exception (TxEndpointsErrorOf s)