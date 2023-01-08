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
import           Server.Endpoints.Tx.Class        (HasTxEndpoints(..), NewTxEndpointResult(..))
import           Server.Class                     (AppM, HasServer(..))
import           Server.Tx                        (mkBalanceTx)
import           Utils.Logger                     (HasLogger(..), (.<))
import           Utils.Servant                    (respondWithStatus)
import           Utils.Tx                         (cardanoTxToText)
import           Utils.ChainIndex                 (MapUTXO)

type NewTxApi s = "relayRequestNewTx"
              :> ReqBody '[JSON] (InputOf s, MapUTXO)
              :> UVerb 'POST '[JSON] (TxApiResultOf s)

newTxHandler :: forall s. HasTxEndpoints s => (InputOf s, MapUTXO) -> AppM s (Union (TxApiResultOf s))
newTxHandler (red, utxosExternal) = handle txEndpointsErrorHanlder $ do
    logMsg $ "New newTx request received:\n" .< red
    checkForTxEndpointsErrors red
    balancedTx <- join $ liftM3 mkBalanceTx (getTrackedAddresses @s) (pure utxosExternal) (txEndpointsTxBuilders @s red)
    case cardanoTxToText balancedTx of
        Just res -> respond $ NewTxEndpointResult res
        Nothing -> respondWithStatus @422 $ "Can't serialise balanced tx:" .< balancedTx