{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module Cardano.Server.Endpoints.Tx.New where

import           Cardano.Server.Endpoints.Tx.Class    (HasTxEndpoints(..))
import           Cardano.Server.Internal              (NetworkM, HasServer(..))
import           Cardano.Server.Tx                    (mkBalanceTx)
import           Cardano.Server.Utils.Logger          (HasLogger(..), (.<))
import           Control.Monad                        (join, liftM3)
import           Control.Monad.Catch                  (Exception, MonadThrow (throwM))
import           Servant                              (JSON, (:>), ReqBody, Post)
import           Utils.Tx                             (cardanoTxToText)
import Cardano.Server.Error
import Ledger (CardanoTx)
import Data.Text (Text)

type NewTxApi s = "newTx"
              :> Throws NewTxApiError
              :> Throws ConnectionError
              :> ReqBody '[JSON] (TxApiRequestOf s)
              :> Post '[JSON] Text

newtype NewTxApiError = UnserialisableBalancedTx CardanoTx
    deriving Show
    deriving Exception via (ExceptionDeriving NewTxApiError)

instance IsCardanoServerError NewTxApiError where
    errStatus _ = toEnum 422
    errMsg (UnserialisableBalancedTx tx) = "Can't serialise balanced tx:" .< tx

newTxHandler :: forall s. HasTxEndpoints s => TxApiRequestOf s
    -> NetworkM s (Envelope '[NewTxApiError, ConnectionError] Text)
newTxHandler req = toEnvelope $ do
    logMsg $ "New newTx request received:\n" .< req
    (input, utxosExternal) <- txEndpointsProcessRequest req
    balancedTx <- join $ liftM3 mkBalanceTx (serverTrackedAddresses @s) (pure utxosExternal) (txEndpointsTxBuilders @s input)
    case cardanoTxToText balancedTx of
        Just res -> pure res
        Nothing -> throwM $ UnserialisableBalancedTx balancedTx