{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Cardano.Server.Example.OnChain where

import           Cardano.Ledger.Alonzo.Language       (Language (PlutusV2))
import           Cardano.Server.Example.Input         (TestPolicyInput, toTokenMap)
import           Ledger                               (Validator, Versioned (..))
import           Ledger.Typed.Scripts                 (IsScriptContext (..))
import           Plutus.Script.Utils.V2.Scripts       (scriptHash)
import           Plutus.Script.Utils.V2.Typed.Scripts (TypedValidator, ValidatorTypes (..), mkTypedValidator, validatorScript)
import           Plutus.V2.Ledger.Api                 (MintingPolicy, ScriptContext (..), ScriptHash, TokenName (..), mkMintingPolicyScript,
                                                       unMintingPolicyScript)
import           PlutusAppsExtra.Constraints.OnChain  (tokensMinted)
import           PlutusTx                             (compile)
import           PlutusTx.Prelude                     (Bool (..), BuiltinByteString, ($))

------------------------------------- Test Minting Policy --------------------------------------

{-# INLINABLE testTokenName #-}
testTokenName :: BuiltinByteString -> TokenName
testTokenName = TokenName

{-# INLINABLE testPolicyCheck #-}
testPolicyCheck :: TestPolicyInput -> ScriptContext -> Bool
testPolicyCheck red ctx = cond1
  where
    cond1 = tokensMinted ctx $ toTokenMap red

testPolicy :: MintingPolicy
testPolicy = mkMintingPolicyScript $$(PlutusTx.compile [|| mkUntypedMintingPolicy testPolicyCheck ||])

testPolicyV :: Versioned MintingPolicy
testPolicyV = Versioned testPolicy PlutusV2

------------------------------------- Test Validator --------------------------------------

data Testing
instance ValidatorTypes Testing where
  type instance DatumType Testing = ()
  type instance RedeemerType Testing = ()

{-# INLINABLE testValidatorCheck #-}
testValidatorCheck :: () -> () -> ScriptContext -> Bool
testValidatorCheck _ _ _ = True

testTypedValidator :: TypedValidator Testing
testTypedValidator = mkTypedValidator @Testing
    $$(PlutusTx.compile [|| testValidatorCheck ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = mkUntypedValidator @ScriptContext @() @()

testScriptHash :: ScriptHash
testScriptHash = scriptHash $ unMintingPolicyScript testPolicy

serverValidator :: Versioned Validator
serverValidator = Versioned (validatorScript testTypedValidator) PlutusV2