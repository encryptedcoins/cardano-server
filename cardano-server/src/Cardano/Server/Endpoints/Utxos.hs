{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Cardano.Server.Endpoints.Utxos where

import qualified CSL
import           CSL.Class                     (toCSL)
import           Cardano.Server.Config         (ServerEndpoint (UtxosE))
import           Cardano.Server.Error          (ConnectionError, CslError (..), Envelope,
                                                IsCardanoServerError (errMsg, errStatus), Throws, toEnvelope)
import           Cardano.Server.Internal       (ServerM, checkEndpointAvailability, getNetworkId)
import           Cardano.Server.Utils.Logger   (logMsg, (.<))
import           Control.Exception             (Exception (..), SomeException, throw)
import           Control.Monad.Catch           (try)
import           Data.Aeson                    (ToJSON)
import           Data.Maybe                    (fromMaybe)
import           Data.Text                     (Text)
import           GHC.Generics                  (Generic)
import           PlutusAppsExtra.IO.ChainIndex (getUtxosAt)
import           PlutusAppsExtra.Utils.Address (bech32ToAddress)
import           Servant                       (Get, JSON, ReqBody, (:>))

type UtxosApi = "utxos"
    :> Throws UtxosError
    :> Throws ConnectionError
    :> Throws CslError
    :> ReqBody '[JSON] Text
    :> Get '[JSON] CSL.TransactionUnspentOutputs

data UtxosError
    = UnparsableAddress 
    deriving (Show, Exception, Generic, ToJSON)

instance IsCardanoServerError UtxosError where
    errStatus _ = toEnum 400
    errMsg = \case
        UnparsableAddress -> "Incorrect wallet address."

utxosHandler :: Text -> ServerM api (Envelope '[UtxosError, ConnectionError, CslError] CSL.TransactionUnspentOutputs)
utxosHandler addrTxt = toEnvelope $ do
    logMsg $ "New utxos request received:\n" .< addrTxt
    checkEndpointAvailability UtxosE
    let !addr = fromMaybe (throw UnparsableAddress) $ bech32ToAddress addrTxt
    networkId <- getNetworkId
    fromMaybe (throw CslConversionError) . toCSL . (, networkId) .
        either mempty id <$> try @_ @SomeException (getUtxosAt addr)