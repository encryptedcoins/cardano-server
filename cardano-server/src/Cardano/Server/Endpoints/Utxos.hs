{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Cardano.Server.Endpoints.Utxos where

import qualified CSL
import           CSL.Class                     (toCSL)
import           Cardano.Server.Error          (ConnectionError, CslError (..), IsCardanoServerError (errMsg, errStatus), Throws)
import           Cardano.Server.Handler        (wrapHandler)
import           Cardano.Server.Internal       (ServerM)
import           Cardano.Server.Utils.Logger   (logMsg, (.<))
import           Control.Exception             (Exception (..), SomeException, throw)
import           Control.Monad.Catch           (try)
import           Data.Aeson                    (ToJSON)
import           Data.Maybe                    (fromMaybe)
import           Data.Text                     (Text)
import           GHC.Generics                  (Generic)
import           PlutusAppsExtra.IO.ChainIndex (HasChainIndexProvider, getUtxosAt)
import           PlutusAppsExtra.Types.Tx      (allRequirements)
import           PlutusAppsExtra.Utils.Address (bech32ToAddress)
import           PlutusAppsExtra.Utils.Network (HasNetworkId (..))
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
    errMsg _ = "Incorrect wallet address."

utxosHandler :: (HasChainIndexProvider (ServerM api), HasNetworkId (ServerM api))
    => Text -> ServerM api CSL.TransactionUnspentOutputs
utxosHandler addrTxt = wrapHandler @UtxosApi $ do
    logMsg $ "New utxos request received:\n" .< addrTxt
    let !addr = fromMaybe (throw UnparsableAddress) $ bech32ToAddress addrTxt
    networkId <- getNetworkId
    fromMaybe (throw CslConversionError) . toCSL . (, networkId) .
        either mempty id <$> try @_ @SomeException (getUtxosAt allRequirements addr)