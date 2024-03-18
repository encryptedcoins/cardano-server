{-# LANGUAGE OverloadedStrings #-}

module Cardano.Server.Endpoints.Submit where

import           Cardano.Server.Error          (ConnectionError, IsCardanoServerError (..), SubmitTxToLocalNodeError, Throws)
import           Cardano.Server.Handler        (wrapHandler)
import           Cardano.Server.Internal       (ServerM)
import           Cardano.Server.Utils.Logger   (logMsg, (.<))
import           Control.Monad.Catch           (Exception, MonadThrow (throwM))
import           Control.Monad.IO.Class        (MonadIO (..))
import           Data.Aeson                    (FromJSON, ToJSON)
import           Data.Either.Extra             (maybeToEither)
import           Data.Text                     (Text)
import           GHC.Generics                  (Generic)
import           Ledger                        (CardanoTx)
import           Ledger.Crypto                 (PubKey, Signature)
import           PlutusAppsExtra.IO.Node       (sumbitTxToNodeLocal)
import           PlutusAppsExtra.Utils.Network (HasNetworkId (getNetworkId))
import           PlutusAppsExtra.Utils.Tx      (addCardanoTxSignature, textToCardanoTx, textToPubkey, textToSignature)
import           Servant                       (JSON, NoContent (..), Post, ReqBody, (:>))

data SubmitTxReqBody = SubmitTxReqBody
    { submitReqTx         :: Text
    , submitReqWitnesses  :: [(Text, Text)]
    } deriving stock    (Show, Read, Generic)
      deriving anyclass (ToJSON, FromJSON)

type SubmitTxApi = "submitTx"
    :> Throws SubmitTxApiError
    :> Throws SubmitTxToLocalNodeError
    :> Throws ConnectionError
    :> ReqBody '[JSON] SubmitTxReqBody
    :> Post '[JSON] NoContent

data SubmitTxApiError
    = UnparsableTx Text
    | UnparsableWitnesses [(Text, Text)]
    deriving stock (Show, Generic)
    deriving anyclass (Exception, ToJSON)

instance IsCardanoServerError SubmitTxApiError where
    errStatus _ = toEnum 400
    errMsg (UnparsableTx tx)          = "Cannot parse CardanoTx from hex:" .< tx
    errMsg (UnparsableWitnesses wtns) = "Cannot parse witnesses from hex:" .< wtns

submitTxHandler :: HasNetworkId (ServerM api) => FilePath -> SubmitTxReqBody -> ServerM api NoContent
submitTxHandler nodeFp req = wrapHandler @SubmitTxApi $ do
    logMsg $ "New submitTx request received:\n" .< req
    (ctx, wtns) <- either throwM pure $ parseSubmitTxReqBody req
    let ctx' = foldr (uncurry addCardanoTxSignature) ctx wtns
    networkId <- getNetworkId
    liftIO (sumbitTxToNodeLocal nodeFp networkId ctx')
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