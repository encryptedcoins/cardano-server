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
import           Server.Error                     (handleUnavailableEndpoints)
import           Server.Internal                  (NetworkM, HasServer(..))
import           Server.Tx                        (mkBalanceTx)
import           Utils.Logger                     (HasLogger(..), (.<))
import           Utils.Tx                         (cardanoTxToText)
import           Server.Endpoints.Servant         (respondWithStatus)

type NewTxApi s = "newTx"
              :> ReqBody '[JSON] (TxApiRequestOf s)
              :> UVerb 'POST '[JSON] (TxApiResultOf s)

newTxHandler :: forall s. HasTxEndpoints s => TxApiRequestOf s-> NetworkM s (Union (TxApiResultOf s))
newTxHandler req = handleUnavailableEndpoints $ handle txEndpointsErrorHandler $ do
    logMsg $ "New newTx request received:\n" .< req
    (input, utxosExternal) <- txEndpointsProcessRequest req
    balancedTx <- join $ liftM3 mkBalanceTx (serverTrackedAddresses @s) (pure utxosExternal) (txEndpointsTxBuilders @s input)
    case cardanoTxToText balancedTx of
        Just res -> respond $ NewTxEndpointResult res
        Nothing -> respondWithStatus @422 $ "Can't serialise balanced tx:" .< balancedTx