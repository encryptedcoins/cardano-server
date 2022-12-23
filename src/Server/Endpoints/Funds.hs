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

import           Control.Monad.Catch    (Exception, handle, throwM)
import           Control.Monad.IO.Class (MonadIO(..))
import           Data.Aeson             (ToJSON)
import           Data.Text              (Text)
import qualified Data.Map               as Map
import           GHC.Generics           (Generic)
import           IO.ChainIndex          (getUtxosAt)
import           Ledger                 (DecoratedTxOut(..))
import           Plutus.V2.Ledger.Api   (Address, CurrencySymbol, TokenName, TxOutRef, Value(..))
import qualified PlutusTx.AssocMap      as PAM
import           Servant                ((:>), StdMethod(GET), JSON, respond, HasStatus,
                                         ReqBody, StatusOf, WithStatus, Union, UVerb)
import           Server.Internal        (AppM, HasServer(..))
import           Utils.Address          (bech32ToAddress)
import           Utils.Logger           (logMsg)
import           Utils.Servant          (respondWithStatus)

type FundsApi = "relayRequestFunds"
               :> ReqBody '[JSON] Text
               :> UVerb 'GET '[JSON] FundsApiResult

type FundsApiResult = '[Funds, WithStatus 400 Text]

newtype Funds = Funds [(TokenName, TxOutRef)]
    deriving (Show, Generic)
    deriving newtype ToJSON

instance HasStatus Funds where
    type StatusOf Funds = 200

data FundsError
    = UnparsableAddress
    deriving (Show, Exception)

fundsHandler :: forall s. HasServer s => Text -> AppM s (Union FundsApiResult)
fundsHandler addrBech32 = handle fundsErrorHandler $ do
    logMsg $ "New funds request received:\n" <> addrBech32
    addr <- maybe (throwM UnparsableAddress) pure $ bech32ToAddress addrBech32
    cs   <- getCurrencySymbol @s
    respond =<< getFunds cs addr

fundsErrorHandler :: FundsError -> AppM s (Union FundsApiResult)
fundsErrorHandler = \case

    UnparsableAddress -> respondWithStatus @400
        "Incorrect wallet address."

getFunds :: MonadIO m => CurrencySymbol -> Address -> m Funds
getFunds cs addr = do
        coins <- liftIO $ Map.toList . Map.map getNames <$> getUtxosAt addr
        pure $ Funds $ concatMap (\(ref, names) -> zip names (repeat ref)) coins
    where
        getNames = maybe [] PAM.keys . PAM.lookup cs . getValue . _decoratedTxOutValue . fst