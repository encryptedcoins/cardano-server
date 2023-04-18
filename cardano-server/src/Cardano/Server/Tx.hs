{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE ViewPatterns       #-}

module Cardano.Server.Tx where

import           Cardano.Server.Error                 (ConnectionError (ConnectionError),
                                                       MkTxError (AllConstructorsFailed, CantExtractKeyHashesFromAddress, NotEnoughFunds),
                                                       throwMaybe)
import           Cardano.Server.Input                 (InputContext (..))
import           Cardano.Server.Internal              (Env (..), ServerM)
import           Cardano.Server.Utils.Logger          (logMsg, logPretty, logSmth)
import           Control.Lens                         ((^?))
import           Control.Monad.Catch                  (Handler (..), MonadThrow (..), catches, handle)
import           Control.Monad.Extra                  (guard, mconcatMapM, void, when)
import           Control.Monad.IO.Class               (MonadIO (..))
import           Control.Monad.Reader                 (asks)
import           Control.Monad.State                  (execState)
import           Data.Aeson                           (decode)
import qualified Data.Aeson                           as J
import           Data.Aeson.Lens                      (AsValue (_String), key)
import           Data.Char                            (isDigit, isSpace)
import           Data.Default                         (def)
import           Data.List.Extra                      (chunksOf, dropPrefix)
import qualified Data.Map                             as Map
import           Data.Maybe                           (fromJust, isNothing, mapMaybe)
import qualified Data.Set                             as Set
import qualified Data.Text                            as T
import           Ledger                               (Address, CardanoTx (..), TxOutRef, onCardanoTx, unspentOutputsTx)
import           Ledger.Ada                           (adaOf, lovelaceValueOf, toValue)
import           Ledger.Constraints                   (TxConstraints (..))
import           Ledger.Constraints.OffChain          (ScriptLookups (..))
import           Ledger.Tx.CardanoAPI                 as CardanoAPI
import           Ledger.Typed.Scripts                 (Any)
import           Ledger.Value                         (CurrencySymbol (..), TokenName (..), Value (..))
import           Network.HTTP.Client                  (HttpExceptionContent (..))
import           PlutusAppsExtra.Constraints.Balance  (balanceExternalTx)
import           PlutusAppsExtra.Constraints.OffChain (useAsCollateralTx', utxoProducedPublicKeyTx)
import           PlutusAppsExtra.IO.ChainIndex        (getUtxosAt)
import           PlutusAppsExtra.IO.Time              (currentTime)
import           PlutusAppsExtra.IO.Wallet            (balanceTx, getWalletAddr, getWalletUtxos, signTx, submitTxConfirmed)
import           PlutusAppsExtra.Types.Tx             (TransactionBuilder, TxConstructor (..), mkTxConstructor,
                                                       selectTxConstructor)
import           PlutusAppsExtra.Utils.Address        (addressToKeyHashes)
import           PlutusAppsExtra.Utils.ChainIndex     (filterCleanUtxos)
import           PlutusTx                             (BuiltinData)
import qualified PlutusTx.AssocMap                    as PAM
import           PlutusTx.Builtins.Class              (ToBuiltin (..))
import           Prettyprinter                        (Doc, Pretty (..), hang, vsep)
import           Servant.Client                       (ClientError (FailureResponse), ResponseF (..))
import           Text.Hex                             (decodeHex)
import           Text.Read                            (readMaybe)

mkBalanceTx :: [Address]
            -> InputContext
            -> [TransactionBuilder ()]
            -> ServerM api CardanoTx
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
    logSmth $ prettyLookups lookups
    logMsg "\tConstraints:"
    logSmth $ prettyCons cons

    logMsg "Balancing..."
    case context of
      InputContextServer {}   -> balanceTx ledgerParams lookups cons
      InputContextClient {..} -> balanceExternalTx ledgerParams inputWalletUTXO inputChangeAddress lookups cons

mkTx :: [Address]
     -> InputContext
     -> [TransactionBuilder ()]
     -> ServerM api CardanoTx
mkTx addressesTracked ctx txs = mkTxErrorH $ do
        balancedTx <- mkBalanceTx addressesTracked ctx txs
        logPretty balancedTx
        logMsg "Signing..."
        signedTx <- signTx balancedTx
        logPretty signedTx
        logMsg "Submitting..."
        handle submitH $ submitTxConfirmed signedTx
        logMsg "Submited."
        return signedTx
    where
        -- | Otherwise we get error on successful submit.
        submitH = \case
            ConnectionError _ NoResponseDataReceived -> pure ()
            err -> throwM err

checkForCleanUtxos :: ServerM api ()
checkForCleanUtxos = mkTxErrorH $ do
    addr       <- getWalletAddr
    cleanUtxos <- length . filterCleanUtxos <$> getWalletUtxos
    minUtxos   <- asks envMinUtxosNumber
    maxUtxos   <- asks envMaxUtxosNumber
    when (cleanUtxos < minUtxos) $ do
        logMsg $ "Address doesn't has enough clean UTXO's: " <> (T.pack . show $ minUtxos - cleanUtxos)
        void $ mkWalletTxOutRefs addr (maxUtxos - cleanUtxos)

mkWalletTxOutRefs :: Address -> Int -> ServerM api [TxOutRef]
mkWalletTxOutRefs addr n = do
    (pkh, scr) <- throwMaybe (CantExtractKeyHashesFromAddress addr) $ addressToKeyHashes addr
    let txBuilder = mapM_ (const $ utxoProducedPublicKeyTx pkh scr (lovelaceValueOf 10_000_000) (Nothing :: Maybe ())) [1..n]
    signedTx <- mkTx [] def [txBuilder]
    let refs =  onCardanoTx (Map.keys . Ledger.unspentOutputsTx) (Map.keys . CardanoAPI.unspentOutputsTx) signedTx
    pure refs

mkTxErrorH :: ServerM api a -> ServerM api a
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

prettyLookups :: ScriptLookups Any -> Doc ann
prettyLookups ScriptLookups{..} = vsep $ map (hang 2 . vsep)
    [ "TxOutputs:"           : (pretty <$> Map.toList slTxOutputs)
    , "OtherScripts:"        : (pretty . show <$> Map.toList slOtherScripts)
    , "OtherData:"           : (pretty <$> Map.toList slOtherData)
    , "PaymentPubKeyHashes:" : (pretty <$> Set.toList slPaymentPubKeyHashes)
    , ["TypedValidator:"      , pretty $ show slTypedValidator]
    , ["OwnPaymentPubKeyHash:", pretty slOwnPaymentPubKeyHash]
    , ["OwnStakingCredential:", pretty slOwnStakingCredential]
    ]

prettyCons :: TxConstraints BuiltinData BuiltinData -> Doc ann
prettyCons TxConstraints{..} = vsep $ map (hang 2 . vsep)
    [ "TxConstraints:"  : (pretty <$> txConstraints)
    , ["TxConstraintFuns", pretty $ show txConstraintFuns]
    , "TxOwnInputs"     : (pretty <$> txOwnInputs)
    , "TxOwnOutputs"    : (pretty <$> txOwnOutputs)
    ]