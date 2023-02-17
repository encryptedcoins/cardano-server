{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE ViewPatterns       #-}

module Cardano.Server.Tx where

import           Cardano.Server.Input                 (InputContext (..))
import           Cardano.Server.Internal              (Env (..))
import           Cardano.Server.Utils.Logger          (HasLogger (..), logPretty, logSmth)
import           Control.Lens                         ((^?))
import           Control.Monad.Catch                  (Handler (..), MonadCatch, MonadThrow (..), catches)
import           Control.Monad.Extra                  (guard, mconcatMapM, void, when)
import           Control.Monad.IO.Class               (MonadIO (..))
import           Control.Monad.Reader                 (MonadReader, asks)
import           Control.Monad.State                  (execState)
import           Data.Aeson                           (decode)
import qualified Data.Aeson                           as J
import           Data.Aeson.Lens                      (AsValue (_String), key)
import           Data.Char                            (isDigit, isSpace)
import           Data.Default                         (def)
import           Data.List.Extra                      (chunksOf, dropPrefix)
import qualified Data.Map                             as Map
import           Data.Maybe                           (fromJust, isNothing, mapMaybe)
import qualified Data.Text                            as T
import           Ledger                               (Address, CardanoTx (..), TxOutRef, onCardanoTx, unspentOutputsTx)
import           Ledger.Ada                           (adaOf, lovelaceValueOf, toValue)
import           Ledger.Tx.CardanoAPI                 as CardanoAPI
import           Ledger.Value                         (CurrencySymbol (..), TokenName (..), Value (..))
import           PlutusAppsExtra.Constraints.Balance  (balanceExternalTx)
import           PlutusAppsExtra.Constraints.OffChain (useAsCollateralTx', utxoProducedPublicKeyTx)
import           PlutusAppsExtra.IO.ChainIndex        (HasChainIndex, getUtxosAt)
import           PlutusAppsExtra.IO.Time              (currentTime)
import           PlutusAppsExtra.IO.Wallet            (HasWallet (..), balanceTx, getWalletAddr, getWalletUtxos, signTx,
                                                       submitTxConfirmed)
import           PlutusAppsExtra.Types.Error          (MkTxError (..), throwMaybe)
import           PlutusAppsExtra.Types.Tx             (TransactionBuilder, TxConstructor (..), mkTxConstructor,
                                                       selectTxConstructor)
import           PlutusAppsExtra.Utils.Address        (addressToKeyHashes)
import           PlutusAppsExtra.Utils.ChainIndex     (filterCleanUtxos)
import qualified PlutusTx.AssocMap                    as PAM
import           PlutusTx.Builtins.Class              (ToBuiltin (..))
import           Servant.Client                       (ClientError (..), ResponseF (..))
import           Text.Hex                             (decodeHex)
import           Text.Read                            (readMaybe)

type MkTxConstraints m s =
    ( HasWallet m
    , HasChainIndex m
    , HasLogger m
    , MonadReader (Env s) m
    , MonadCatch m
    )

mkBalanceTx :: MkTxConstraints m s
    => [Address]
    -> InputContext
    -> [TransactionBuilder ()]
    -> m CardanoTx
mkBalanceTx addressesTracked context txs = do
    utxosTracked <- mconcatMapM getUtxosAt addressesTracked
    ct           <- liftIO currentTime
    ledgerParams <- asks envLedgerParams
    collateral   <- asks envCollateral
    let utxos = utxosTracked `Map.union` inputUTXO context
        txs'  = map (useAsCollateralTx' collateral >>) txs
    let constrInit = mkTxConstructor ct utxos
        constrs = map (`execState` constrInit) txs'
        constr = selectTxConstructor constrs
    when (isNothing constr) $ throwM $ AllConstructorsFailed $ concatMap txConstructorErrors constrs
    let (lookups, cons) = fromJust $ txConstructorResult $ fromJust constr
    logMsg "\tLookups:"
    logSmth lookups
    logMsg "\tConstraints:"
    logSmth cons

    logMsg "Balancing..."
    case context of
      InputContextServer {}   -> balanceTx ledgerParams lookups cons
      InputContextClient {..} -> balanceExternalTx ledgerParams inputWalletUTXO inputChangeAddress lookups cons

mkTx :: MkTxConstraints m s
    => [Address]
    -> InputContext
    -> [TransactionBuilder ()]
    -> m CardanoTx
mkTx addressesTracked ctx txs = mkTxErrorH $ do
    balancedTx <- mkBalanceTx addressesTracked ctx txs
    logPretty balancedTx
    logMsg "Signing..."
    signedTx <- signTx balancedTx
    logPretty signedTx
    logMsg "Submitting..."
    submitTxConfirmed signedTx
    logMsg "Submited."
    return signedTx

checkForCleanUtxos :: MkTxConstraints m s => m ()
checkForCleanUtxos = mkTxErrorH $ do
    addr       <- getWalletAddr
    cleanUtxos <- length . filterCleanUtxos <$> getWalletUtxos
    minUtxos   <- asks envMinUtxosAmount
    when (cleanUtxos < minUtxos) $ do
        logMsg $ "Address doesn't has enough clean UTXO's: " <> (T.pack . show $ minUtxos - cleanUtxos)
        void $ mkWalletTxOutRefs addr (minUtxos - cleanUtxos)

mkWalletTxOutRefs :: MkTxConstraints m s => Address -> Int -> m [TxOutRef]
mkWalletTxOutRefs addr n = do
    (pkh, scr) <- throwMaybe (CantExtractKeyHashesFromAddress addr) $ addressToKeyHashes addr
    let txBuilder = mapM_ (const $ utxoProducedPublicKeyTx pkh scr (lovelaceValueOf 10_000_000) (Nothing :: Maybe ())) [1..n]
    signedTx <- mkTx [] def [txBuilder]
    let refs =  onCardanoTx (Map.keys . Ledger.unspentOutputsTx) (Map.keys . CardanoAPI.unspentOutputsTx) signedTx
    pure refs

mkTxErrorH :: MonadCatch m => m a -> m a
mkTxErrorH = (`catches` [clientErrorH])
    where
        clientErrorH = Handler $ \case
            FailureResponse _ (getLackingFundsFromFailedResponse -> Just val) -> throwM $ NotEnoughFunds val
            e -> throwM e
        getLackingFundsFromFailedResponse r = do
            body <- decode @J.Value $ responseBody r
            guard $ body ^? key "code" == Just "not_enough_money"
            msg <- body ^? key "message" . _String
            ada <- fmap adaOf . readMaybe . T.unpack . T.takeWhile (not . isSpace) . T.dropWhile (not . isDigit) $ msg
            let triples = filter (not . null) $ chunksOf 3 $ map T.stripStart $ drop 1 $ T.splitOn "\n" $ mconcat $ drop 1 $ T.splitOn "tokens:" msg
                toBbs = fmap toBuiltin . decodeHex . T.pack
                fromTriple [policy, token, quantity] = do
                    policy' <- fmap CurrencySymbol $ toBbs $ dropPrefix "- policy: " policy
                    token' <- case drop 1 $ dropPrefix "token:" token of
                        "" -> Just ""
                        xs -> TokenName <$> toBbs xs
                    quantity' <- readMaybe $ dropPrefix "quantity: " quantity
                    pure $ Value $ PAM.singleton policy' (PAM.singleton token' quantity')
                fromTriple _ = Nothing
            pure $ mconcat $ (toValue ada :) $ mapMaybe (fromTriple . map T.unpack) triples