{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

module Cardano.Server.Endpoints.Tx.New where

import           Cardano.Server.Config             (isInactiveNewTx)
import           Cardano.Server.Endpoints.Tx.Class (HasTxEndpoints (..))
import           Cardano.Server.Error              (ConnectionError, Envelope, ExceptionDeriving (..), IsCardanoServerError (..),
                                                    Throws, toEnvelope)
import           Cardano.Server.Internal           (HasServer (..), NetworkM, checkEndpointAvailability)
import           Cardano.Server.Tx                 (mkBalanceTx)
import           Cardano.Server.Utils.Logger       (HasLogger (..), (.<))
import           Control.Monad                     (join, liftM3)
import           Control.Monad.Catch               (Exception, MonadThrow (throwM))
import           Data.Aeson                        (ToJSON)
import           Data.Text                         (Text)
import           GHC.Generics                      (Generic)
import           Ledger                            (CardanoTx, getCardanoTxId, TxId (..))
import           PlutusAppsExtra.Types.Error       (MkTxError)
import           PlutusAppsExtra.Utils.Tx          (cardanoTxToText)
import           PlutusTx.Prelude                  (fromBuiltin)
import           Servant                           (JSON, Post, ReqBody, (:>))
import           Text.Hex                          (encodeHex)

type NewTxApi s = "newTx"
              :> Throws NewTxApiError
              :> Throws ConnectionError
              :> Throws MkTxError
              :> ReqBody '[JSON] (TxApiRequestOf s)
              :> Post '[JSON] (Text, Text)

newtype NewTxApiError = UnserialisableCardanoTx CardanoTx
    deriving (Show, Generic, ToJSON)
    deriving Exception via (ExceptionDeriving NewTxApiError)

instance IsCardanoServerError NewTxApiError where
    errStatus _ = toEnum 422
    errMsg (UnserialisableCardanoTx tx) = "Cannot serialise balanced tx:" .< tx

newTxHandler :: forall s. HasTxEndpoints s
    => TxApiRequestOf s
    -> NetworkM s (Envelope '[NewTxApiError, ConnectionError, MkTxError] (Text, Text))
newTxHandler req = toEnvelope $ do
    logMsg $ "New newTx request received:\n" .< req
    checkEndpointAvailability isInactiveNewTx
    (input, context) <- txEndpointsProcessRequest req
    balancedTx <- join $ liftM3 mkBalanceTx (serverTrackedAddresses @s) (pure context) (txEndpointsTxBuilders @s input)
    case cardanoTxToText balancedTx of
        Just res ->
            let txId = encodeHex $ fromBuiltin $ getTxId $ getCardanoTxId balancedTx
            in pure (txId, res)
        Nothing  -> throwM $ UnserialisableCardanoTx balancedTx