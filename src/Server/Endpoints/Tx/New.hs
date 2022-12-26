{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

module Server.Endpoints.Tx.New where

import           Control.Monad                    (join, liftM3)
import           Control.Monad.Catch              (handle)
import qualified Data.Map                         as Map
import           Ledger                           (TxOutRef, DecoratedTxOut)
import           Servant                          (JSON, (:>), ReqBody, respond, StdMethod(POST), UVerb, Union)
import           Server.Endpoints.Tx.Internal     (HasTxEndpoints(..), NewTxEndpointResult(..))
import           Server.Internal                  (AppM, HasServer(..))
import           Server.Tx                        (mkBalanceTx)
import           Utils.Logger                     (HasLogger(..), (.<))
import           Utils.Servant                    (respondWithStatus)
import           Utils.Tx                         (cardanoTxToText)

type NewTxApi s = "relayRequestNewTx"
              :> ReqBody '[JSON] (RedeemerOf s, Map.Map TxOutRef DecoratedTxOut)
              :> UVerb 'POST '[JSON] (TxApiResultOf s)

newTxHandler :: forall s. HasTxEndpoints s => (RedeemerOf s, Map.Map TxOutRef DecoratedTxOut) -> AppM s (Union (TxApiResultOf s))
newTxHandler (red, utxosExternal) = handle txEndpointsErrorHanlder $ do
    logMsg $ "New newTx request received:\n" .< red
    checkForTxEndpointsErros red
    balancedTx <- join $ liftM3 mkBalanceTx (getTrackedAddresses @s) (pure utxosExternal) (txEndpointsTxBuilders @s red)
    case cardanoTxToText balancedTx of
        Just res -> respond $ NewTxEndpointResult res
        Nothing -> respondWithStatus @422 $ "Can't serialise balanced tx:" .< balancedTx