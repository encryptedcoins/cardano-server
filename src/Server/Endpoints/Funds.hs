{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Server.Endpoints.Funds where

import           Control.Exception      (throw)
import           Control.Monad.Catch    (Exception, handle, throwM)
import           Control.Monad.IO.Class (MonadIO(..))
import           Data.Aeson             (ToJSON, FromJSON)
import           Data.Text              (Text)
import qualified Data.Map               as Map
import           GHC.Generics           (Generic)
import           IO.ChainIndex          (getUtxosAt)
import           Ledger                 (DecoratedTxOut(..))
import           Plutus.V2.Ledger.Api   (Address, CurrencySymbol (CurrencySymbol), TokenName, TxOutRef, Value(..))
import qualified PlutusTx.AssocMap      as PAM
import           PlutusTx.Builtins      (toBuiltin)
import           Servant                ((:>), StdMethod(GET), JSON, respond, HasStatus,
                                         ReqBody, StatusOf, WithStatus, Union, UVerb)
import           Server.Internal        (NetworkM)
import           Text.Hex               (decodeHex)
import           Utils.Address          (bech32ToAddress)
import           Utils.Logger           (logMsg)
import           Utils.Servant          (respondWithStatus)
import Server.Error (handleUnavailableEndpoints)

data FundsReqBody = FundsReqBody
    {
        fundsReqAddress :: Text,
        fundsReqCS      :: Text
    }
    deriving (Show, Generic, ToJSON, FromJSON)

type FundsApi = "relayRequestFunds"
               :> ReqBody '[JSON] FundsReqBody
               :> UVerb 'GET '[JSON] FundsApiResult

type FundsApiResult = '[Funds, WithStatus 400 Text, WithStatus 503 Text]

newtype Funds = Funds [(TokenName, TxOutRef)]
    deriving (Show, Generic)
    deriving newtype ToJSON

instance HasStatus Funds where
    type StatusOf Funds = 200

data FundsError
    = UnparsableAddress | UnparsableCurrencySymbol
    deriving (Show, Exception)

fundsHandler :: FundsReqBody -> NetworkM s (Union FundsApiResult)
fundsHandler (FundsReqBody addrBech32 csHex) = fundsErrorHandler $ do
    logMsg $ "New funds request received:\n" <> addrBech32
    addr <- maybe (throwM UnparsableAddress) pure $ bech32ToAddress addrBech32
    let cs = CurrencySymbol $ maybe (throw UnparsableCurrencySymbol) toBuiltin $ decodeHex csHex
    respond =<< getFunds cs addr

fundsErrorHandler :: forall s. NetworkM s (Union FundsApiResult) -> NetworkM s (Union FundsApiResult)
fundsErrorHandler = handleUnavailableEndpoints @s . handle (\case
    UnparsableAddress        -> respondWithStatus @400
        "Incorrect wallet address."
    UnparsableCurrencySymbol -> respondWithStatus @400
        "Incorrect currency symbol.")

getFunds :: MonadIO m => CurrencySymbol -> Address -> m Funds
getFunds cs addr = do
        coins <- liftIO $ Map.toList . Map.map getNames <$> getUtxosAt addr
        pure $ Funds $ concatMap (\(ref, names) -> zip names (repeat ref)) coins
    where
        getNames = maybe [] PAM.keys . PAM.lookup cs . getValue . _decoratedTxOutValue