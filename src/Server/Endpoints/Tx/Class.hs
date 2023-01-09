{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}

module Server.Endpoints.Tx.Class where

import           Control.Monad.Catch              (Exception)
import           Control.Monad.Reader             (MonadReader)
import           Data.Kind                        (Type)
import           Data.Text                        (Text)
import           IO.Wallet                        (HasWallet(..))
import           Servant                          (NoContent(..), Union, IsMember, WithStatus)
import           Server.Class                     (NetworkM, HasServer(..), Env)
import           Server.Endpoints.Tx.Internal     (NewTxEndpointResult)
import           Types.Tx                         (TransactionBuilder)

class ( HasServer s
      , IsMember NoContent             (TxApiResultOf s)
      , IsMember NewTxEndpointResult   (TxApiResultOf s)
      , IsMember (WithStatus 422 Text) (TxApiResultOf s)
      , Show (TxEndpointsErrorOf s)
      , Exception (TxEndpointsErrorOf s)
      ) => HasTxEndpoints s where

    type TxApiResultOf s :: [Type]

    data TxEndpointsErrorOf s

    txEndpointsTxBuilders :: (MonadReader (Env s) m, HasWallet m) => InputOf s -> m [TransactionBuilder ()]

    checkForTxEndpointsErrors :: InputOf s -> NetworkM s ()

    txEndpointsErrorHandler :: TxEndpointsErrorOf s -> NetworkM s (Union (TxApiResultOf s))