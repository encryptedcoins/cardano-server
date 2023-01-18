{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Cardano.Server.Endpoints.AddSignature where

import           Cardano.Server.Error             (ExceptionDeriving(..), Envelope, IsCardanoServerError(..), Throws,
                                                   toEnvelope)
import           Cardano.Server.Internal          (NetworkM)
import           Cardano.Server.Utils.Logger      (HasLogger(..), (.<))
import           Control.Monad.Catch              (Exception, MonadThrow (..))
import           Data.Text                        (Text)
import           Servant                          (JSON, (:>), ReqBody, Post)
import           Utils.Tx                         (textToCardanoTx)

type AddSignatureApi s = "addSignature"
              :> Throws AddSignatureError
              :> ReqBody '[JSON] AddSignatureReqBody
              :> Post '[JSON] Text

type AddSignatureReqBody = (Text, Text)

data AddSignatureError 
    = UnparsableTx Text 
    | UnparsableSignature Text
    deriving Show
    deriving Exception via ExceptionDeriving AddSignatureError

instance IsCardanoServerError AddSignatureError where
    errStatus _ = toEnum 400
    errMsg = \case 
        UnparsableTx tx -> "Cannot deserialise to CardanoTx:" .< tx
        UnparsableSignature sig -> "Cannot deserialise to CardanoTx:" .< sig

addSignatureHandler :: AddSignatureReqBody -> NetworkM s (Envelope '[AddSignatureError] Text)
addSignatureHandler req@(tx, sig) = toEnvelope $ do
    logMsg $ "New AddSignature request received:\n" .< req
    case textToCardanoTx tx of
        Just _ -> do
            logMsg tx
            logMsg sig
            pure "Adding signature here..."
        Nothing  -> throwM $ UnparsableTx tx