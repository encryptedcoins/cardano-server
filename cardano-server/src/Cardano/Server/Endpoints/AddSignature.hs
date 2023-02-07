{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Cardano.Server.Endpoints.AddSignature where

import           Cardano.Server.Config            (isInactiveAddSignature)
import           Cardano.Server.Error             (ExceptionDeriving(..), Envelope, IsCardanoServerError(..), Throws,
                                                   toEnvelope)
import           Cardano.Server.Internal          (NetworkM, checkEndpointAvailability)
import           Cardano.Server.Utils.Logger      (HasLogger(..), (.<))
import           Control.Monad.Catch              (Exception, MonadThrow (..))
import           Data.Aeson                       (ToJSON)
import           Data.Text                        (Text)
import           GHC.Generics                     (Generic)
import           Servant                          (JSON, (:>), ReqBody, Post)
import           PlutusAppsExtra.Utils.Tx         (textToCardanoTx)

type AddSignatureApi s = "addSignature"
              :> Throws AddSignatureError
              :> ReqBody '[JSON] AddSignatureReqBody
              :> Post '[JSON] Text

type AddSignatureReqBody = (Text, Text)

data AddSignatureError 
    = UnparsableTx Text 
    | UnparsableSignature Text
    deriving (Show, Generic, ToJSON)
    deriving Exception via ExceptionDeriving AddSignatureError

instance IsCardanoServerError AddSignatureError where
    errStatus _ = toEnum 400
    errMsg = \case 
        UnparsableTx tx -> "Cannot deserialise to CardanoTx:" .< tx
        UnparsableSignature sig -> "Cannot deserialise to CardanoTx:" .< sig

addSignatureHandler :: AddSignatureReqBody -> NetworkM s (Envelope '[AddSignatureError] Text)
addSignatureHandler req@(tx, sig) = toEnvelope $ do
    logMsg $ "New AddSignature request received:\n" .< req
    checkEndpointAvailability isInactiveAddSignature
    case textToCardanoTx tx of
        Just _ -> do
            logMsg tx
            logMsg sig
            pure "Adding signature here..."
        Nothing  -> throwM $ UnparsableTx tx