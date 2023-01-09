{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

module Server.Endpoints.Tx.New where

import           Control.Monad                    (join, liftM3)
import           Control.Monad.Catch              (handle)
import           Servant                          (JSON, (:>), ReqBody, respond, StdMethod(POST), UVerb, Union)
import           Server.Endpoints.Tx.Class        (HasTxEndpoints(..))
import           Server.Endpoints.Tx.Internal     (NewTxEndpointResult(..))
import           Server.Class                     (NetworkM, HasServer(..))
import           Server.Tx                        (mkBalanceTx)
import           Utils.Logger                     (HasLogger(..), (.<))
import           Utils.Servant                    (respondWithStatus)
import           Utils.Tx                         (cardanoTxToText)
import           Utils.ChainIndex                 (MapUTXO)

type NewTxApi s = "relayRequestNewTx"
              :> ReqBody '[JSON] (InputOf s, MapUTXO)
              :> UVerb 'POST '[JSON] (TxApiResultOf s)

newTxHandler :: forall s. HasTxEndpoints s => (InputOf s, MapUTXO) -> NetworkM s (Union (TxApiResultOf s))
newTxHandler (red, utxosExternal) = handle txEndpointsErrorHandler $ do
    logMsg $ "New newTx request received:\n" .< red
    checkForTxEndpointsErrors red
    balancedTx <- join $ liftM3 mkBalanceTx (serverTrackedAddresses @s) (pure utxosExternal) (txEndpointsTxBuilders @s red)
    case cardanoTxToText balancedTx of
        Just res -> respond $ NewTxEndpointResult res
        Nothing -> respondWithStatus @422 $ "Can't serialise balanced tx:" .< balancedTx