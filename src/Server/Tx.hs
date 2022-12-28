{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Tx where

import           Cardano.Api.Shelley       (NetworkMagic(..), NetworkId(..))
import           Control.Monad             (when)
import           Control.Monad.Extra       (mconcatMapM)
import           Control.Monad.State       (State, get, put, execState, MonadIO(..))
import           Data.Default              (Default(..))
import qualified Data.Map                  as Map
import           Data.Maybe                (fromJust, isNothing)
import           Data.Void                 (Void)
import           Ledger                    (Address, CardanoTx(..), DecoratedTxOut, Params(..), POSIXTime, PubKeyHash, TxOutRef, PaymentPubKeyHash (..), StakingCredential, stakingCredential, pParamsFromProtocolParams)
import           Ledger.Ada                (lovelaceValueOf) 
import           Ledger.Constraints        (ScriptLookups, mustPayToPubKey, mustPayToPubKeyAddress)
import           Ledger.Tx.CardanoAPI      (unspentOutputsTx)
import           PlutusTx                  (FromData(..), ToData(..))
import           Plutus.Script.Utils.Typed (RedeemerType, DatumType)
import           IO.ChainIndex             (getUtxosAt)
import           IO.Time                   (currentTime)
import           IO.Wallet                 (HasWallet(..), signTx, balanceTx, submitTxConfirmed, getWalletAddr,
                                            getWalletAddrBech32, getWalletKeyHashes)
import           Server.Config             (decodeOrErrorFromFile)
import           Types.Tx                  (TxConstructor (..), selectTxConstructor, mkTxConstructor)
import           Utils.Logger              (HasLogger(..), logPretty, logSmth)

type HasTxEnv =
    ( ?txWalletAddr :: Address
    , ?txWalletPKH  :: PubKeyHash
    , ?txWalletSKC  :: Maybe StakingCredential
    , ?txCt         :: POSIXTime
    , ?txUtxos      :: Map.Map TxOutRef DecoratedTxOut
    , ?txParams     :: Params
    )

type MkTxConstraints a m =
    ( FromData (DatumType a)
    , ToData   (DatumType a)
    , ToData   (RedeemerType a)
    , Show     (DatumType a)
    , Show     (RedeemerType a)
    , HasWallet m
    , HasLogger m
    )

mkBalanceTx :: forall a m. MkTxConstraints a m 
    => [Address]
    -> Map.Map TxOutRef DecoratedTxOut
    -> (HasTxEnv => [State (TxConstructor a (RedeemerType a) (DatumType a)) ()])
    -> m CardanoTx
mkBalanceTx addressesTracked utxosExternal txs = do
    walletAddrBech32       <- getWalletAddrBech32
    walletAddr             <- getWalletAddr
    (walletPKH, walletSKH) <- getWalletKeyHashes
    utxosTracked           <- liftIO $ mconcatMapM getUtxosAt addressesTracked
    ct                     <- liftIO currentTime
    params                 <- liftIO $ decodeOrErrorFromFile "testnet/protocol-parameters.json"
    let utxos = utxosTracked `Map.union` utxosExternal

    let networkId = Testnet $ NetworkMagic 2
        ledgerParams = Params def (pParamsFromProtocolParams params) networkId

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
    let (lookups, cons) = fromJust $ txConstructorResult $ fromJust constr
    logMsg "\tLookups:"
    logSmth lookups
    logMsg "\tConstraints:"
    logSmth cons

    logMsg "Balancing..."
    balanceTx ledgerParams lookups cons

mkTx :: forall a m. MkTxConstraints a m 
    => [Address]
    -> Map.Map TxOutRef DecoratedTxOut
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

mkWalletTxOutRefs :: MkTxConstraints Void m => Address -> Int -> m [TxOutRef]
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
