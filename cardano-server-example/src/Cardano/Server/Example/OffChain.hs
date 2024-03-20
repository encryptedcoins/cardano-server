{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.Server.Example.OffChain where

import           Cardano.Server.Example.Input         (TestPolicyInput (..))
import           Cardano.Server.Example.OnChain       (testPolicy, testPolicyV, testTokenName)
import           Control.Monad.State                  (State)
import           Ledger.Tokens                        (token)
import           Ledger.Typed.Scripts                 (Any)
import           Plutus.Script.Utils.V2.Scripts       (scriptCurrencySymbol)
import           Plutus.Script.Utils.V2.Typed.Scripts (ValidatorTypes (..))
import           Plutus.Script.Utils.Value            (AssetClass (..))
import qualified Plutus.Script.Utils.Value            as Value
import           Plutus.V2.Ledger.Api                 (CurrencySymbol, TokenName (..), Value)
import           PlutusAppsExtra.Constraints.OffChain (tokensMintedTx)
import           PlutusAppsExtra.Types.Tx             (TxConstructor (..))
import           PlutusTx.Prelude                     (BuiltinByteString, mconcat, zipWith, ($), (.))

type TestTransaction = TxConstructor Any (RedeemerType Any) (DatumType Any)
type TestTransactionBuilder = State TestTransaction ()

------------------------------------- Testing Minting Policy --------------------------------------

testCurrencySymbol :: CurrencySymbol
testCurrencySymbol = scriptCurrencySymbol testPolicy

testAssetClass :: BuiltinByteString -> AssetClass
testAssetClass bs = AssetClass (testCurrencySymbol, testTokenName bs)

testToken :: BuiltinByteString -> Value
testToken = token . testAssetClass

testMintTx :: TestPolicyInput-> TestTransactionBuilder
testMintTx arg@(TestPolicyInput bbss amts) = tokensMintedTx testPolicyV arg $ mconcat $ zipWith f amts bbss
  where
    f amt bbs = Value.singleton testCurrencySymbol (TokenName bbs) amt