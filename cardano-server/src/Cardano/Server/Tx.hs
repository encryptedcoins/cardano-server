{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE ViewPatterns       #-}

module Cardano.Server.Tx where

import           Cardano.Node.Emulator                (Params)
import           Cardano.Server.Error                 (ConnectionError (ConnectionError), MkTxError (..), throwMaybe)
import           Cardano.Server.Input                 (InputContext (..))
import           Cardano.Server.Internal              (Env (..), ServerM)
import           Cardano.Server.Utils.Logger          (HasLogger, logMsg, logPretty, logSmth)
import           Control.Lens                         ((^?))
import           Control.Monad.Catch                  (Handler (..), MonadCatch, MonadThrow (..), catches, handle)
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
import           Ledger                               (Address, TxOutRef)
import           Ledger.Tx.CardanoAPI                 as CardanoAPI (CardanoTx, fromCardanoValue, unspentOutputsTx)
import           Ledger.Tx.Constraints                (ScriptLookups (..), TxConstraints (..))
import           Ledger.Typed.Scripts                 (Any)
import qualified Ledger.Value.CardanoAPI              as C
import           Network.HTTP.Client                  (HttpExceptionContent (..))
import qualified Plutus.Script.Utils.Ada              as P
import           Plutus.V2.Ledger.Api                 (CurrencySymbol (..), TokenName (..))
import qualified Plutus.V2.Ledger.Api                 as P
import           PlutusAppsExtra.Constraints.Balance  (balanceExternalTx)
import           PlutusAppsExtra.Constraints.OffChain (useAsCollateralTx', utxoProducedPublicKeyTx)
import           PlutusAppsExtra.IO.ChainIndex        (getUtxosAt)
import           PlutusAppsExtra.IO.Time              (currentTime)
import qualified PlutusAppsExtra.IO.Wallet
import           PlutusAppsExtra.Types.Tx             (TransactionBuilder, TxConstructor (..), mkTxConstructor,
                                                       selectTxConstructor, txBuilderRequirements)
import           PlutusAppsExtra.Utils.Address        (addressToKeyHashes)
import           PlutusAppsExtra.Utils.ChainIndex     (filterCleanUtxos)
import           PlutusTx                             (BuiltinData)
import qualified PlutusTx.AssocMap                    as PAM
import           PlutusTx.Builtins.Class              (ToBuiltin (..))
import           Prettyprinter                        (Doc, Pretty (..), hang, vsep)
import           Servant.Client                       (ClientError (FailureResponse), ResponseF (..))
import           Text.Hex                             (decodeHex)
import           Text.Read                            (readMaybe)
import qualified PlutusAppsExtra.IO.Tx
import PlutusAppsExtra.IO.Tx (HasTxProvider)

type MkTxConstrains m = (MonadIO m, MonadCatch m, HasLogger m, HasTxProvider m, HasMkTxEnv m)

class HasMkTxEnv m where
    getCollateral   :: m (Maybe TxOutRef)
    getLedgerParams :: m Params

instance HasMkTxEnv (ServerM api) where
    getCollateral   = asks envCollateral
    getLedgerParams = asks envLedgerParams

mkBalanceTx :: MkTxConstrains m
    => [Address]
    -> InputContext
    -> [TransactionBuilder ()]
    -> m CardanoTx
mkBalanceTx addressesTracked context txs = do
    reqs         <- liftIO $ txBuilderRequirements txs
    utxosTracked <- mconcatMapM (getUtxosAt reqs) addressesTracked
    ct           <- liftIO currentTime
    ledgerParams <- getLedgerParams
    collateral   <- getCollateral
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
        InputContextServer {}   -> PlutusAppsExtra.IO.Tx.balanceTx ledgerParams lookups cons
        InputContextClient {..} -> balanceExternalTx ledgerParams inputWalletUTXO inputChangeAddress lookups cons

signTx :: MkTxConstrains m
    => [Address]
    -> InputContext
    -> [TransactionBuilder ()]
    -> m CardanoTx
signTx addressesTracked ctx txs = mkTxErrorH $ do
    balancedTx <- mkBalanceTx addressesTracked ctx txs
    logPretty balancedTx
    logMsg "Signing..."
    signedTx <- PlutusAppsExtra.IO.Tx.signTx balancedTx
    logPretty signedTx
    return signedTx

submitTx :: MkTxConstrains m
    => [Address]
    -> InputContext
    -> [TransactionBuilder ()]
    -> m CardanoTx
submitTx addressesTracked ctx txs = mkTxErrorH $ do
    signedTx <- signTx addressesTracked ctx txs
    logMsg "Submitting..."
    PlutusAppsExtra.IO.Tx.submitTx signedTx
    return signedTx

awaitTxConfirmed :: MkTxConstrains m => CardanoTx -> m CardanoTx
awaitTxConfirmed ctx = mkTxErrorH $ do
        handle submitH $ PlutusAppsExtra.IO.Tx.awaitTxConfirmed ctx
        logMsg "Submited."
        return ctx
    where
        -- | Otherwise we get error on successful submit.
        submitH = \case
            ConnectionError _ NoResponseDataReceived -> pure ()
            err -> throwM err

mkTx :: MkTxConstrains m
    => [Address]
    -> InputContext
    -> [TransactionBuilder ()]
    -> m CardanoTx
mkTx addressesTracked ctx txs = submitTx addressesTracked ctx txs >>= awaitTxConfirmed

checkForCleanUtxos :: ServerM api ()
checkForCleanUtxos = mkTxErrorH $ do
    logMsg "Checking for clean utxos..."
    addr       <- PlutusAppsExtra.IO.Wallet.getWalletAddr
    logMsg "Got wallet address."
    cleanUtxos <- length . filterCleanUtxos <$> PlutusAppsExtra.IO.Wallet.getWalletUtxos mempty
    minUtxos   <- asks envMinUtxosNumber
    maxUtxos   <- asks envMaxUtxosNumber
    logMsg "Got utxos."
    when (cleanUtxos < minUtxos) $ do
        logMsg $ "Address doesn't has enough clean UTXO's: " <> (T.pack . show $ minUtxos - cleanUtxos)
        void $ mkWalletTxOutRefs addr (maxUtxos - cleanUtxos)
    logMsg "Finished checking for clean utxos."

mkWalletTxOutRefs :: MkTxConstrains m => Address -> Int -> m [TxOutRef]
mkWalletTxOutRefs addr n = do
    (pkh, scr) <- throwMaybe (CantExtractKeyHashesFromAddress addr) $ addressToKeyHashes addr
    let txBuilder = mapM_ (const $ utxoProducedPublicKeyTx pkh scr (fromCardanoValue $ C.lovelaceValueOf 10_000_000) Nothing) [1..n]
    signedTx <- mkTx [] def [txBuilder]
    let refs = Map.keys . CardanoAPI.unspentOutputsTx $ signedTx
    pure refs

mkTxErrorH :: MkTxConstrains m => m a -> m a
mkTxErrorH = (`catches` [clientErrorH])
    where
        clientErrorH = Handler $ \case
            FailureResponse _ (getLackingFundsFromFailedResponse -> Just val) -> throwM $ NotEnoughFunds val
            e -> throwM e
        getLackingFundsFromFailedResponse r = do
            body <- decode @J.Value $ responseBody r
            guard $ body ^? key "code" == Just "not_enough_money"
            msg <- body ^? key "message" . _String
            ada <- fmap P.adaOf . readMaybe . T.unpack . T.takeWhile (not . isSpace) . T.dropWhile (not . isDigit) $ msg
            let triples = filter (not . null) $ chunksOf 3 $ map T.stripStart $ drop 1 $ T.splitOn "\n" $ mconcat $ drop 1 $ T.splitOn "tokens:" msg
                toBbs = fmap toBuiltin . decodeHex . T.pack
                fromTriple [policy, token, quantity] = do
                    policy' <- fmap CurrencySymbol $ toBbs $ dropPrefix "- policy: " policy
                    token' <- case drop 1 $ dropPrefix "token:" token of
                        "" -> Just ""
                        xs -> TokenName <$> toBbs xs
                    quantity' <- readMaybe $ dropPrefix "quantity: " quantity
                    pure $ P.Value $ PAM.singleton policy' (PAM.singleton token' quantity')
                fromTriple _ = Nothing
            pure $ mconcat $ (P.toValue ada :) $ mapMaybe (fromTriple . map T.unpack) triples

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