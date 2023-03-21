{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Cardano.Server.Endpoints.Tx.Submit where

import           Cardano.Node.Emulator                (Params (..))
import           Cardano.Server.Config                (isInactiveSubmitTx)
import           Cardano.Server.Endpoints.Tx.Internal (TxApiErrorOf)
import           Cardano.Server.Error                 (ConnectionError, Envelope, IsCardanoServerError (..),
                                                       SubmitTxToLocalNodeError, Throws, toEnvelope)
import           Cardano.Server.Internal              (Env (..), ServerM, checkEndpointAvailability)
import           Cardano.Server.Utils.Logger          (logMsg, (.<))
import           Control.Monad.Catch                  (Exception, MonadThrow (throwM))
import           Control.Monad.IO.Class               (MonadIO (..))
import           Control.Monad.Reader                 (asks)
import           Data.Aeson                           (FromJSON, ToJSON)
import           Data.Either.Extra                    (maybeToEither)
import           Data.Text                            (Text)
import           GHC.Generics                         (Generic)
import           Ledger                               (CardanoTx)
import           Ledger.Crypto                        (PubKey, Signature)
import           PlutusAppsExtra.IO.Node              (sumbitTxToNodeLocal)
import           PlutusAppsExtra.Utils.Tx             (addCardanoTxSignature, textToCardanoTx, textToPubkey, textToSignature)
import           Servant                              (JSON, NoContent (..), Post, ReqBody, (:>))

data SubmitTxReqBody = SubmitTxReqBody
    {
        submitReqTx         :: Text,
        submitReqWitnesses  :: [(Text, Text)]
    }
    deriving (Show, Read, Generic, ToJSON, FromJSON)

type SubmitTxApi err = "submitTx"
    :> Throws err
    :> Throws SubmitTxApiError
    :> Throws SubmitTxToLocalNodeError
    :> Throws ConnectionError
    :> ReqBody '[JSON] SubmitTxReqBody
    :> Post '[JSON] NoContent

data SubmitTxApiError = UnparsableTx Text
                      | UnparsableWitnesses [(Text, Text)]
    deriving (Show, Generic, ToJSON)
    deriving Exception

instance IsCardanoServerError SubmitTxApiError where
    errStatus _ = toEnum 400
    errMsg (UnparsableTx tx)          = "Cannot parse CardanoTx from hex:" .< tx
    errMsg (UnparsableWitnesses wtns) = "Cannot parse witnesses from hex:" .< wtns

submitTxHandler :: IsCardanoServerError (TxApiErrorOf api)
    => SubmitTxReqBody 
    -> ServerM api (Envelope '[TxApiErrorOf api, SubmitTxApiError, SubmitTxToLocalNodeError, ConnectionError] NoContent)
submitTxHandler req = toEnvelope $ do
    logMsg $ "New submitTx request received:\n" .< req
    checkEndpointAvailability isInactiveSubmitTx
    (ctx, wtns) <- either throwM pure $ parseSubmitTxReqBody req
    let ctx' = foldr (uncurry addCardanoTxSignature) ctx wtns
    networkId <- asks $ pNetworkId . envLedgerParams
    node      <- asks envNodeFilePath
    liftIO (sumbitTxToNodeLocal node networkId ctx')
    pure NoContent

parseSubmitTxReqBody :: SubmitTxReqBody -> Either SubmitTxApiError (CardanoTx, [(PubKey, Signature)])
parseSubmitTxReqBody SubmitTxReqBody{..} = do
        ctx  <- maybeToEither (UnparsableTx submitReqTx) $ textToCardanoTx submitReqTx
        wtns <- maybeToEither (UnparsableWitnesses submitReqWitnesses) $ mapM parseWitness submitReqWitnesses
        pure (ctx, wtns)
    where
        parseWitness (pkText, sigText) = do
            pk  <- textToPubkey pkText
            sig <- textToSignature sigText
            return (pk, sig)