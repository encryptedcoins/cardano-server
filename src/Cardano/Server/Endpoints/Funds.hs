{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Cardano.Server.Endpoints.Funds where

import           Cardano.Server.Internal          (NetworkM)
import           Cardano.Server.Utils.Logger      (logMsg)
import           Cardano.Server.Endpoints.Servant (respondWithStatus)
import           Cardano.Server.Error             (handleUnavailableEndpoints)
import           Control.Exception                (throw)
import           Control.Monad.Catch              (Exception, handle, throwM)
import           Control.Monad.IO.Class           (MonadIO(..))
import           Data.Aeson                       (ToJSON, FromJSON)
import           Data.Text                        (Text)
import qualified Data.Map                         as Map
import           GHC.Generics                     (Generic)
import           IO.ChainIndex                    (getUtxosAt)
import           Ledger                           (DecoratedTxOut(..))
import           Plutus.V2.Ledger.Api             (Address, CurrencySymbol (CurrencySymbol), TokenName, TxOutRef, Value(..))
import qualified PlutusTx.AssocMap                as PAM
import           PlutusTx.Builtins                (toBuiltin)
import           Servant                          ((:>), StdMethod(GET), JSON, respond, HasStatus, ReqBody, WithStatus,
                                                   Union, UVerb)
import           Text.Hex                         (decodeHex)
import           Utils.Address                    (bech32ToAddress)

data FundsReqBody = FundsReqBody
    {
        fundsReqAddress :: Text,
        fundsReqCS      :: Text
    }
    deriving (Show, Generic, ToJSON, FromJSON)

type FundsApi = "funds"
               :> ReqBody '[JSON] FundsReqBody
               :> UVerb 'GET '[JSON] FundsApiResult

type FundsApiResult = '[Funds, WithStatus 400 Text, WithStatus 503 Text]

newtype Funds = Funds [(TokenName, TxOutRef)]
    deriving (Show, Generic)
    deriving newtype ToJSON
    deriving HasStatus via WithStatus 200 Funds

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