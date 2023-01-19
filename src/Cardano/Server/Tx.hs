{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Server.Tx where

import           Control.Exception           (Exception)
import           Cardano.Server.Error        (ExceptionDeriving(..), IsCardanoServerError(..))
import           Cardano.Server.Internal     (Env(..))
import           Cardano.Server.Utils.Logger (HasLogger(..), logPretty, logSmth)
import           Control.Monad.Catch         (MonadThrow(..))
import           Control.Monad.Extra         (mconcatMapM, when, void)
import           Control.Monad.IO.Class      (MonadIO(..))
import           Control.Monad.Reader        (MonadReader, asks)
import           Control.Monad.State         (State, get, put, execState)
import           Data.Aeson                  (ToJSON)
import qualified Data.Map                    as Map
import           Data.Maybe                  (fromJust, isNothing)
import           Data.Void                   (Void)
import           GHC.Generics                (Generic)
import           Ledger                      (Address, CardanoTx(..), Params(..), POSIXTime, PubKeyHash, TxOutRef,
                                              PaymentPubKeyHash(..), StakingCredential, stakingCredential)
import           Ledger.Ada                  (lovelaceValueOf) 
import           Ledger.Constraints          (ScriptLookups, mustPayToPubKey, mustPayToPubKeyAddress)
import           Ledger.Tx.CardanoAPI        (unspentOutputsTx)
import           PlutusTx                    (FromData(..), ToData(..))
import           Plutus.Script.Utils.Typed   (RedeemerType, DatumType)
import           IO.ChainIndex               (getUtxosAt, getWalletUtxos)
import           IO.Time                     (currentTime)
import           IO.Wallet                   (HasWallet(..), signTx, balanceTx, submitTxConfirmed, getWalletAddr,
                                              getWalletAddrBech32, getWalletKeyHashes)
import           Types.Tx                    (TxConstructor (..), selectTxConstructor, mkTxConstructor)
import           Utils.ChainIndex            (MapUTXO, filterCleanUtxos)

type HasTxEnv =
    ( ?txWalletAddr :: Address
    , ?txWalletPKH  :: PubKeyHash
    , ?txWalletSKC  :: Maybe StakingCredential
    , ?txCt         :: POSIXTime
    , ?txUtxos      :: MapUTXO
    , ?txParams     :: Params
    )

type MkTxConstraints a m s =
    ( FromData (DatumType a)
    , ToData   (DatumType a)
    , ToData   (RedeemerType a)
    , Show     (DatumType a)
    , Show     (RedeemerType a)
    , HasWallet m
    , HasLogger m
    , MonadReader (Env s) m
    , MonadThrow m
    )

data MkTxError = UnbuildableTx
    deriving (Show, Generic, ToJSON)
    deriving Exception via (ExceptionDeriving MkTxError) 

instance IsCardanoServerError MkTxError where
    errStatus _ = toEnum 422
    errMsg _ = "The requested transaction could not be built."

mkBalanceTx :: MkTxConstraints a m s
    => [Address]
    -> MapUTXO
    -> (HasTxEnv => [State (TxConstructor a (RedeemerType a) (DatumType a)) ()])
    -> m CardanoTx
mkBalanceTx addressesTracked utxosExternal txs = do
    walletAddrBech32       <- getWalletAddrBech32
    walletAddr             <- getWalletAddr
    (walletPKH, walletSKH) <- getWalletKeyHashes
    utxosTracked           <- liftIO $ mconcatMapM getUtxosAt addressesTracked
    ct                     <- liftIO currentTime
    ledgerParams           <- asks envLedgerParams
    let utxos = utxosTracked `Map.union` utxosExternal

    let ?txWalletAddr = walletAddr
        ?txWalletPKH  = unPaymentPubKeyHash walletPKH
        ?txWalletSKC  = stakingCredential walletAddr
        ?txCt         = ct
        ?txUtxos      = utxos
        ?txParams     = ledgerParams

    logMsg $ "Wallet address:\n" <> walletAddrBech32

    let constrInit = mkTxConstructor
            (walletPKH, walletSKH)
            ct
            utxos
        constr = selectTxConstructor $ map (`execState` constrInit) txs
    when (isNothing constr) $ do
        logMsg "\tNo transactions can be constructed. Last error:"
        logSmth $ head $ txConstructorErrors $ last $ map (`execState` constrInit) txs
        throwM UnbuildableTx
    let (lookups, cons) = fromJust $ txConstructorResult $ fromJust constr
    logMsg "\tLookups:"
    logSmth lookups
    logMsg "\tConstraints:"
    logSmth cons

    logMsg "Balancing..."
    balanceTx ledgerParams lookups cons

mkTx :: MkTxConstraints a m s
    => [Address]
    -> MapUTXO
    -> (HasTxEnv => [State (TxConstructor a (RedeemerType a) (DatumType a)) ()])
    -> m CardanoTx
mkTx addressesTracked utxosExternal txs = do
    balancedTx <- mkBalanceTx addressesTracked utxosExternal txs
    logPretty balancedTx
    logMsg "Signing..."
    signedTx <- signTx balancedTx
    logPretty signedTx
    logMsg "Submitting..."
    submitTxConfirmed signedTx
    logMsg "Submited."
    return signedTx

checkForCleanUtxos :: MkTxConstraints Void m s => m ()
checkForCleanUtxos = do
    addr       <- getWalletAddr
    cleanUtxos <- length . filterCleanUtxos <$> getWalletUtxos
    minUtxos   <- asks envMinUtxosAmount
    when (cleanUtxos < minUtxos) $ do
        logMsg "Address doesn't has enough clean UTXO's."
        void $ mkWalletTxOutRefs addr (cleanUtxos - minUtxos)

mkWalletTxOutRefs :: MkTxConstraints Void m s => Address -> Int -> m [TxOutRef]
mkWalletTxOutRefs addr n = do
        signedTx <- mkTx [addr] Map.empty [constructor]
        let refs = case signedTx of
                EmulatorTx _    -> error "Can not get TxOutRef's from EmulatorTx."
                CardanoApiTx tx -> Map.keys $ unspentOutputsTx tx
        pure refs
    where
        constructor :: HasTxEnv => State (TxConstructor Void i o) ()
        constructor = do
            let pkh = PaymentPubKeyHash ?txWalletPKH
                cons = case ?txWalletSKC of
                    Just skc -> mconcat $ replicate n $ mustPayToPubKeyAddress pkh skc $ lovelaceValueOf 10_000_000
                    Nothing  -> mustPayToPubKey pkh $ lovelaceValueOf 10_000_000
            constr <- get
            put constr { txConstructorResult = Just (mempty :: ScriptLookups Void, cons) }