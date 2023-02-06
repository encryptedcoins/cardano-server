{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE ViewPatterns       #-}

module Cardano.Server.Tx where

import           Cardano.Server.Input        (InputContext (..))
import           Cardano.Server.Internal     (Env (..))
import           Cardano.Server.Utils.Logger (HasLogger (..), logPretty, logSmth)
import           Constraints.Balance         (balanceExternalTx)
import           Constraints.OffChain        (utxoProducedPublicKeyTx)
import           Control.Monad.Catch         (Handler (..), MonadCatch, MonadThrow (..), catches)
import           Control.Monad.Extra         (mconcatMapM, void, when, guard)
import           Control.Monad.IO.Class      (MonadIO (..))
import           Control.Monad.Reader        (MonadReader, asks)
import           Control.Monad.State         (execState)
import           Data.Default                (def)
import qualified Data.Map                    as Map
import           Data.Maybe                  (fromJust, isNothing)
import           IO.ChainIndex               (getUtxosAt)
import           IO.Time                     (currentTime)
import           IO.Wallet                   (HasWallet (..), balanceTx, getWalletAddr, getWalletUtxos, signTx, submitTxConfirmed)
import           Ledger                      (Address, CardanoTx (..), TxOutRef, onCardanoTx, unspentOutputsTx)
import           Ledger.Ada                  (adaOf, lovelaceValueOf)
import           Ledger.Tx.CardanoAPI        as CardanoAPI
import           Servant.Client              (ClientError (..), ResponseF (..))
import           Types.Error                 (MkTxError (..), throwMaybe)
import           Types.Tx                    (TransactionBuilder, TxConstructor (..), mkTxConstructor, selectTxConstructor)
import           Utils.Address               (addressToKeyHashes)
import           Utils.ChainIndex            (filterCleanUtxos)
import           Control.Lens                ((^?))
import           Data.Aeson                  (Value, decode)
import           Data.Aeson.Lens             (AsValue (_String), key)
import           Data.Char                   (isDigit, isSpace)
import           Data.Functor                ((<&>))
import qualified Data.Text                   as T
import           Text.Read                   (readMaybe)

type MkTxConstraints m s =
    ( HasWallet m
    , HasLogger m
    , MonadReader (Env s) m
    , MonadCatch m
    )

-- TODO: implement two types of balancing
mkBalanceTx :: MkTxConstraints m s
    => [Address]
    -> InputContext
    -> [TransactionBuilder ()]
    -> m CardanoTx
mkBalanceTx addressesTracked context txs = do
    utxosTracked <- liftIO $ mconcatMapM getUtxosAt addressesTracked
    ct           <- liftIO currentTime
    ledgerParams <- asks envLedgerParams
    let utxos = utxosTracked `Map.union` inputUTXO context

    let constrInit = mkTxConstructor ct utxos
        constr = selectTxConstructor $ map (`execState` constrInit) txs
    when (isNothing constr) $ do
        logMsg "\tNo transactions can be constructed. Last error:"
        logSmth $ head $ txConstructorErrors $ last $ map (`execState` constrInit) txs
        throwM AllConstructorsFailed
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
        logMsg "Address doesn't has enough clean UTXO's."
        void $ mkWalletTxOutRefs addr (cleanUtxos - minUtxos)

mkWalletTxOutRefs :: MkTxConstraints m s => Address -> Int -> m [TxOutRef]
mkWalletTxOutRefs addr n = do
    (pkh, scr) <- throwMaybe (CantExtractKeyHashesFromAddress addr) $ addressToKeyHashes addr
    let txBuilder = mapM_ (const $ utxoProducedPublicKeyTx pkh scr (lovelaceValueOf 10_000_000) (Nothing :: Maybe ())) [1..n]
    signedTx <- mkTx [addr] def [txBuilder]
    let refs =  onCardanoTx (Map.keys . Ledger.unspentOutputsTx) (Map.keys . CardanoAPI.unspentOutputsTx) signedTx
    pure refs

mkTxErrorH :: MonadCatch m => m a -> m a
mkTxErrorH = (`catches` [failedReponseH])
    where
        failedReponseH = Handler $ \case
            FailureResponse _ (getLackingFundsFromFailedResponse -> Just ada) -> throwM $ NotEnoughFunds ada
            e -> throwM e
        getLackingFundsFromFailedResponse r = do
            body <- decode @Value $ responseBody r
            guard $ body  ^? key "code" == Just "not_enough_money" 
            ada  <- body  ^? key "message" . _String <&> T.unpack . T.takeWhile (not . isSpace) . T.dropWhile (not . isDigit)
            adaOf <$> readMaybe ada