{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Cardano.Server.Endpoints.Funds where

import           Cardano.Server.Config         (isInactiveFunds)
import           Cardano.Server.Error          (ConnectionError, Envelope, IsCardanoServerError (errMsg, errStatus), Throws,
                                                toEnvelope)
import           Cardano.Server.Internal       (NetworkM, checkEndpointAvailability)
import           Cardano.Server.Utils.Logger   (logMsg)
import           Control.Exception             (Exception (..), throw)
import           Data.Aeson                    (FromJSON, ToJSON)
import qualified Data.Map                      as Map
import           Data.Maybe                    (fromMaybe)
import           Data.Text                     (Text)
import           GHC.Generics                  (Generic)
import           Ledger                        (DecoratedTxOut (..))
import           Plutus.V2.Ledger.Api          (Address, CurrencySymbol (CurrencySymbol), TokenName, TxOutRef, Value (..))
import           PlutusAppsExtra.IO.ChainIndex (HasChainIndex, getUtxosAt)
import           PlutusAppsExtra.Utils.Address (bech32ToAddress)
import qualified PlutusTx.AssocMap             as PAM
import           PlutusTx.Builtins             (toBuiltin)
import           Servant                       (Get, HasStatus, JSON, ReqBody, WithStatus, (:>))
import           Text.Hex                      (decodeHex)

data FundsReqBody = FundsReqBody
    {
        fundsReqAddress :: Text,
        fundsReqCS      :: Text
    }
    deriving (Show, Generic, ToJSON, FromJSON)

type FundsApi = "funds"
    :> Throws FundsError
    :> Throws ConnectionError
    :> ReqBody '[JSON] FundsReqBody
    :> Get '[JSON] Funds

newtype Funds = Funds [(TokenName, TxOutRef)]
    deriving (Show, Generic)
    deriving newtype ToJSON
    deriving HasStatus via WithStatus 200 Funds

data FundsError
    = UnparsableAddress | UnparsableCurrencySymbol
    deriving (Show, Generic, ToJSON)
    deriving Exception

instance IsCardanoServerError FundsError where
    errStatus _ = toEnum 400
    errMsg = \case
        UnparsableAddress        -> "Incorrect wallet address."
        UnparsableCurrencySymbol -> "Incorrect currency symbol."

fundsHandler :: FundsReqBody -> NetworkM s (Envelope '[FundsError, ConnectionError] Funds)
fundsHandler (FundsReqBody addrBech32 csHex) = toEnvelope $ do
    logMsg $ "New funds request received:\n" <> addrBech32
    checkEndpointAvailability isInactiveFunds
    let cs   =  maybe (throw UnparsableCurrencySymbol) (CurrencySymbol . toBuiltin) $ decodeHex csHex
        addr =  fromMaybe (throw UnparsableAddress) $ bech32ToAddress addrBech32
    getFunds cs addr

getFunds :: HasChainIndex m => CurrencySymbol -> Address -> m Funds
getFunds cs addr = do
        coins <- Map.toList . Map.map getNames <$> getUtxosAt addr
        pure $ Funds $ concatMap (\(ref, names) -> zip names (repeat ref)) coins
    where
        getNames = maybe [] PAM.keys . PAM.lookup cs . getValue . _decoratedTxOutValue