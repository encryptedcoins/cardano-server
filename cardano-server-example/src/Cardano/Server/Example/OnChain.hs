{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Cardano.Server.Example.OnChain where

import           Cardano.Ledger.Alonzo.Language      (Language (PlutusV2))
-- import           Cardano.Server.Example.Input        (TestPolicyInput (..))
import           Ledger                              (Versioned (..))
import           Ledger.Typed.Scripts                (IsScriptContext (..))
import           Plutus.V2.Ledger.Api                (MintingPolicy, ScriptContext (..), TokenName (..), mkMintingPolicyScript)
import           PlutusAppsExtra.Constraints.OnChain (tokensMinted)
import           PlutusTx                            (compile)
import qualified PlutusTx.AssocMap                   as AssocMap
import           PlutusTx.Prelude                    (Bool (..), BuiltinByteString, Integer, ($), (<$>))

------------------------------------- Test Minting Policy --------------------------------------

{-# INLINABLE toTokenMap #-}
toTokenMap :: [(BuiltinByteString, Integer)] -> AssocMap.Map TokenName Integer
toTokenMap i = AssocMap.fromList $ (\(bbs, amt) -> (TokenName bbs, amt)) <$> i

{-# INLINABLE testTokenName #-}
testTokenName :: BuiltinByteString -> TokenName
testTokenName = TokenName

{-# INLINABLE testPolicyCheck #-}
testPolicyCheck :: [(BuiltinByteString, Integer)] -> ScriptContext -> Bool
testPolicyCheck red ctx = cond1
  where
    cond1 = tokensMinted ctx $ toTokenMap red

testPolicy :: MintingPolicy
testPolicy = mkMintingPolicyScript $$(PlutusTx.compile [|| mkUntypedMintingPolicy testPolicyCheck ||])

testPolicyV :: Versioned MintingPolicy
testPolicyV = Versioned testPolicy PlutusV2