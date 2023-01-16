{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}

module Cardano.Server.Endpoints.Tx.Class where

import           Cardano.Server.Endpoints.Tx.Internal (NewTxEndpointResult)
import           Cardano.Server.Internal              (NetworkM, HasServer(..), Env)
import           Control.Monad.Catch                  (Exception)
import           Control.Monad.Reader                 (MonadReader)
import           Data.Kind                            (Type)
import           Data.Text                            (Text)
import           IO.Wallet                            (HasWallet(..))
import           Servant                              (NoContent(..), Union, IsMember, WithStatus)
import           Types.Tx                             (TransactionBuilder)
import           Utils.ChainIndex                     (MapUTXO)

class ( HasServer s
      , IsMember NoContent             (TxApiResultOf s)
      , IsMember NewTxEndpointResult   (TxApiResultOf s)
      , IsMember (WithStatus 422 Text) (TxApiResultOf s)
      , IsMember (WithStatus 503 Text) (TxApiResultOf s)
      , Show (TxApiRequestOf s)
      , Show (TxEndpointsErrorOf s)
      , Exception (TxEndpointsErrorOf s)
      ) => HasTxEndpoints s where

    type TxApiRequestOf s :: Type

    type TxApiResultOf s  :: [Type]

    data TxEndpointsErrorOf s

    txEndpointsProcessRequest :: TxApiRequestOf s -> NetworkM s (InputOf s, MapUTXO)

    txEndpointsTxBuilders     :: (MonadReader (Env s) m, HasWallet m) => InputOf s -> m [TransactionBuilder ()]

    txEndpointsErrorHandler   :: TxEndpointsErrorOf s -> NetworkM s (Union (TxApiResultOf s))
