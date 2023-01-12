{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveAnyClass #-}

module Server.Endpoints.AddSignature where

import           Control.Monad.Catch              (Exception, handle)
import           Data.Text                        (Text)
import           Servant                          (JSON, (:>), ReqBody, StdMethod(POST), UVerb, Union, WithStatus)
import           Server.Endpoints.Servant         (respondWithStatus)
import           Server.Class                     (AppM)
import           Utils.Logger                     (HasLogger(..), (.<))
import           Utils.Tx                         (textToCardanoTx)

type AddSignatureReqBody = (Text, Text)

type AddSignatureApi s = "addSignature"
              :> ReqBody '[JSON] AddSignatureReqBody
              :> UVerb 'POST '[JSON] AddSignatureApiResult

type AddSignatureApiResult = '[WithStatus 200 Text, WithStatus 400 Text]

data AddSignatureError = UnparsableTx | UnparsableSignature
    deriving (Show, Exception)

addSignatureHandler :: AddSignatureReqBody -> AppM s (Union AddSignatureApiResult)
addSignatureHandler req@(tx, sig) = handle addSignatureErrorHandler $ do
    logMsg $ "New AddSignature request received:\n" .< req
    case textToCardanoTx tx of
      Just _ -> do
        logMsg tx
        logMsg sig
        respondWithStatus @400 $ "Adding signature here..."
      Nothing  -> respondWithStatus @400 $ "Cannot deserialise to CardanoTx:" .< tx

addSignatureErrorHandler :: AddSignatureError -> AppM s (Union AddSignatureApiResult)
addSignatureErrorHandler = \case
    UnparsableTx        -> respondWithStatus @400
        "Incorrect wallet address."
    UnparsableSignature -> respondWithStatus @400
        "Incorrect currency symbol."