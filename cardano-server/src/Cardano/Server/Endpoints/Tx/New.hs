{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE EmptyDataDeriving     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Cardano.Server.Endpoints.Tx.New where

import           Cardano.Server.Config                (ServerEndpoint (NewTxE))
import           Cardano.Server.Endpoints.Tx.Internal (TxApiErrorOf)
import           Cardano.Server.Error                 (BalanceExternalTxError, ConnectionError, Envelope,
                                                       IsCardanoServerError (..), MkTxError, Throws, toEnvelope)
import           Cardano.Server.Internal              (ServerM, TxApiRequestOf, checkEndpointAvailability, serverTrackedAddresses,
                                                       txEndpointProcessRequest, txEndpointsTxBuilders)
import           Cardano.Server.Tx                    (mkBalanceTx)
import           Cardano.Server.Utils.Logger          (logMsg, (.<))
import           Control.Monad.Catch                  (Exception, MonadThrow (throwM))
import           Control.Monad.IO.Class               (MonadIO (..))
import           Data.Aeson                           (ToJSON)
import           Data.Text                            (Text)
import qualified Data.Time                            as Time
import           GHC.Generics                         (Generic)
import           Ledger                               (CardanoTx, TxId (..), getCardanoTxId)
import           PlutusAppsExtra.Utils.Tx             (cardanoTxToText)
import           PlutusTx.Prelude                     (fromBuiltin)
import           Servant                              (JSON, Post, ReqBody, type (:>))
import           Text.Hex                             (encodeHex)

type NewTxApi reqBody err = "newTx"
    :> Throws err
    :> Throws NewTxApiError
    :> Throws ConnectionError
    :> Throws MkTxError
    :> Throws BalanceExternalTxError
    :> ReqBody '[JSON] reqBody
    :> Post '[JSON] (Text, Text)

newtype NewTxApiError = UnserialisableCardanoTx CardanoTx
    deriving stock    (Show, Generic)
    deriving anyclass (ToJSON, Exception)

instance IsCardanoServerError NewTxApiError where
    errStatus _ = toEnum 422
    errMsg (UnserialisableCardanoTx tx) = "Cannot serialise balanced tx:" .< tx

newTxHandler :: (Show (TxApiRequestOf api), IsCardanoServerError (TxApiErrorOf api))
    => TxApiRequestOf api
    -> ServerM api (Envelope
        [TxApiErrorOf api, NewTxApiError, ConnectionError, MkTxError, BalanceExternalTxError]
        (Text, Text))
newTxHandler req = withMetric "newTx request processing" $ toEnvelope $ do
    logMsg $ "New newTx request received:\n" .< req
    checkEndpointAvailability NewTxE
    (input, context) <- withMetric "processing request" $
        txEndpointProcessRequest req
    balancedTx       <- withMetric "balancing tx" $ do
        addrs <- serverTrackedAddresses
        txBuilder <- txEndpointsTxBuilders input
        mkBalanceTx addrs context txBuilder Nothing
    case cardanoTxToText balancedTx of
        Just res ->
            let txId = encodeHex $ fromBuiltin $ getTxId $ getCardanoTxId balancedTx
            in pure (txId, res)
        Nothing  -> throwM $ UnserialisableCardanoTx balancedTx
    where
        withMetric msg ma = do
            logMsg $ "start " <> msg
            start  <- liftIO Time.getCurrentTime
            res    <- ma
            finish <- res `seq` liftIO Time.getCurrentTime
            logMsg $ msg <> " finished in " .< Time.diffUTCTime finish start
            pure res