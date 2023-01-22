{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

module Cardano.Server.Endpoints.Tx.Submit where

import           Cardano.Server.Error                 (ConnectionError, Envelope, Throws, IsCardanoServerError(..),
                                                       ExceptionDeriving(..), toEnvelope)
import           Cardano.Server.Internal              (NetworkM)
import           Cardano.Server.Utils.Logger          (HasLogger(..), (.<))
import           Control.Monad.Catch                  (Exception, MonadThrow (throwM))
import           Data.Aeson                           (ToJSON, FromJSON)
import           Data.Text                            (Text)
import           GHC.Generics                         (Generic)
import           IO.Wallet                            (submitTx)
import           Ledger.Crypto                        (PubKey, Signature)
import           Servant                              (JSON, (:>), ReqBody, Post, NoContent (..))
import           Utils.Tx                             (textToCardanoTx, textToPubkey, textToSignature, addCardanoTxSignature)

data SubmitTxReqBody = SubmitTxReqBody
    {
        submitReqTx         :: Text,
        submitReqWitnesses  :: [(Text, Text)]
    }
    deriving (Show, Generic, ToJSON, FromJSON)

type SubmitTxApi s = "submitTx"
              :> Throws SubmitTxApiError
              :> Throws ConnectionError
              :> ReqBody '[JSON] SubmitTxReqBody
              :> Post '[JSON] NoContent

data SubmitTxApiError = UnparsableTx Text
                        | UnparsableWitnesses [(Text, Text)]
    deriving (Show, Generic, ToJSON)
    deriving Exception via (ExceptionDeriving SubmitTxApiError)

instance IsCardanoServerError SubmitTxApiError where
    errStatus _ = toEnum 400
    errMsg (UnparsableTx tx)          = "Cannot parse CardanoTx from hex:" .< tx
    errMsg (UnparsableWitnesses wtns) = "Cannot parse witnesses from hex:" .< wtns

submitTxHandler :: SubmitTxReqBody
    -> NetworkM s (Envelope '[SubmitTxApiError, ConnectionError] NoContent)
submitTxHandler req@(SubmitTxReqBody tx wtnsText) = toEnvelope $ do
    logMsg $ "New submitTx request received:\n" .< req
    case textToCardanoTx tx of
        Nothing  -> throwM $ UnparsableTx tx
        Just ctx -> case mapM parseWitness wtnsText of
            Nothing   -> throwM $ UnparsableWitnesses wtnsText
            Just wtns ->
                let ctx' = foldr (uncurry addCardanoTxSignature) ctx wtns
                in submitTx ctx' >> pure NoContent
    where
        parseWitness :: (Text, Text) -> Maybe (PubKey, Signature)
        parseWitness (pkText, sigText) = do
            pk  <- textToPubkey pkText
            sig <- textToSignature sigText
            return (pk, sig)
