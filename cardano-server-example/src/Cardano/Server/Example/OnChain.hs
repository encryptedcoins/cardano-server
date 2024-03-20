{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Cardano.Server.Example.OnChain where

import           Cardano.Ledger.Alonzo.Language      (Language (PlutusV2))
import           Ledger                              (Versioned (..))
import           Ledger.Typed.Scripts                (IsScriptContext (..))
import           Plutus.V2.Ledger.Api                (MintingPolicy, ScriptContext (..), TokenName (..), mkMintingPolicyScript)
import           PlutusAppsExtra.Constraints.OnChain (tokensMinted)
import           PlutusTx                            (compile)
import           PlutusTx.Builtins.Class             (stringToBuiltinString)
import           PlutusTx.Prelude                    (Bool (..), BuiltinByteString, Integer, emptyByteString, emptyString, ($))
import           PlutusTx.Trace                      (traceIfFalse)

------------------------------------- Test Minting Policy --------------------------------------

data TestPolicyInput = TestPolicyInput [BuiltinByteString] [Integer]
unstableMakeIsData ''TestPolicyInput

{-# INLINABLE toTokenMap #-}
toTokenMap :: TestPolicyInput -> AssocMap.Map TokenName Integer
toTokenMap (TestPolicyInput bbss amts)
    = AssocMap.fromList $ sort $ zipWith (\bbs amt -> (TokenName bbs, amt)) bbss amts

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