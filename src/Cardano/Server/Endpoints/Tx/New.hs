{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

module Cardano.Server.Endpoints.Tx.New where

import           Cardano.Server.Endpoints.Servant     (respondWithStatus)
import           Cardano.Server.Endpoints.Tx.Class    (HasTxEndpoints(..))
import           Cardano.Server.Endpoints.Tx.Internal (NewTxEndpointResult(..))
import           Cardano.Server.Error                 (handleUnavailableEndpoints)
import           Cardano.Server.Internal              (NetworkM, HasServer(..))
import           Cardano.Server.Tx                    (mkBalanceTx)
import           Cardano.Server.Utils.Logger          (HasLogger(..), (.<))
import           Control.Monad                        (join, liftM3)
import           Control.Monad.Catch                  (handle)
import           Servant                              (JSON, (:>), ReqBody, respond, StdMethod(POST), UVerb, Union)
import           Utils.Tx                             (cardanoTxToText)

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