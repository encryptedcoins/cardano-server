{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Cardano.Server.Endpoints.Funds where

import           Cardano.Server.Config         (isInactiveFunds)
import           Cardano.Server.Error          (ConnectionError, Envelope, IsCardanoServerError (errMsg, errStatus), Throws,
                                                toEnvelope)
import           Cardano.Server.Internal       (ServerM, checkEndpointAvailability)
import           Cardano.Server.Utils.Logger   (logMsg)
import           Control.Exception             (Exception (..))
import           Control.Monad.Catch           (MonadThrow (throwM))
import           Data.Aeson                    (FromJSON, ToJSON)
import           Data.Either.Extra             (maybeToEither)
import qualified Data.Map                      as Map
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
    deriving (Show, Read, Generic, ToJSON, FromJSON)

type FundsApi = "funds"
    :> Throws FundsError
    :> Throws ConnectionError
    :> ReqBody '[JSON] FundsReqBody
    :> Get '[JSON] Funds

newtype Funds = Funds [(TokenName, TxOutRef)]
    deriving (Show, Eq, Generic)
    deriving newtype (ToJSON, FromJSON)
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

fundsHandler :: FundsReqBody -> ServerM api (Envelope '[FundsError, ConnectionError] Funds)
fundsHandler frb@(FundsReqBody addrBech32 _) = toEnvelope $ do
    logMsg $ "New funds request received:\n" <> addrBech32
    checkEndpointAvailability isInactiveFunds
    either throwM (uncurry getFunds) $ parseFundsReqBody frb

parseFundsReqBody :: FundsReqBody -> Either FundsError (CurrencySymbol, Address)
parseFundsReqBody FundsReqBody{..} = do
    cs   <- maybeToEither UnparsableCurrencySymbol $ decodeHex fundsReqCS
    addr <- maybeToEither UnparsableAddress $ bech32ToAddress fundsReqAddress
    pure (CurrencySymbol $ toBuiltin cs, addr)

getFunds :: HasChainIndex m => CurrencySymbol -> Address -> m Funds
getFunds cs addr = do
        coins <- Map.toList . Map.map getNames <$> getUtxosAt addr
        pure $ Funds $ concatMap (\(ref, names) -> zip names (repeat ref)) coins
    where
        getNames = maybe [] PAM.keys . PAM.lookup cs . getValue . _decoratedTxOutValue