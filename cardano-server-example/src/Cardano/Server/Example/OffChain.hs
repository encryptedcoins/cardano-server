{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Cardano.Server.Example.OffChain where

import           Cardano.Server.Example.OnChain       (testPolicy, testPolicyV, testTokenName, testTypedValidator)
import           Control.Monad.State                  (State)
import           Ledger.Tokens                        (token)
import           Ledger.Typed.Scripts                 (Any)
import           Plutus.Script.Utils.V2.Scripts       (ValidatorHash, scriptCurrencySymbol, validatorHash)
import           Plutus.Script.Utils.V2.Typed.Scripts (ValidatorTypes (..), validatorScript)
import           Plutus.Script.Utils.Value            (AssetClass (..))
import           Plutus.V2.Ledger.Api                 (CurrencySymbol, Validator, Value)
import           PlutusAppsExtra.Constraints.OffChain (tokensMintedTx)
import           PlutusAppsExtra.Types.Tx             (TxConstructor (..))
import           PlutusTx.Prelude

type TestTransaction = TxConstructor Any (RedeemerType Any) (DatumType Any)
type TestTransactionBuilder = State TestTransaction ()

------------------------------------- Testing Minting Policy --------------------------------------

testCurrencySymbol :: CurrencySymbol
testCurrencySymbol = scriptCurrencySymbol testPolicy

testAssetClass :: BuiltinByteString -> AssetClass
testAssetClass bs = AssetClass (testCurrencySymbol, testTokenName bs)

testToken :: BuiltinByteString -> Value
testToken = token . testAssetClass

testMintTx :: [BuiltinByteString] -> TestTransactionBuilder
testMintTx bss = tokensMintedTx testPolicyV bss (sum $ map testToken bss)

------------------------------------- Testing Validator --------------------------------------

testValidator :: Validator
testValidator = validatorScript testTypedValidator

testValidatorHash :: ValidatorHash
testValidatorHash = validatorHash testValidator