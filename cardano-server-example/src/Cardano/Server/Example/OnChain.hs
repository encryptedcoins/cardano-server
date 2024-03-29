{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TupleSections              #-}

module Cardano.Server.Example.OnChain where

import           Cardano.Ledger.Alonzo.Language       (Language(PlutusV2))
import           PlutusAppsExtra.Constraints.OnChain  (tokensMinted)
import           Ledger                               (Versioned(..), Validator)
import           Ledger.Typed.Scripts                 (IsScriptContext(..))
import           Plutus.Script.Utils.V2.Typed.Scripts (ValidatorTypes (..), TypedValidator, mkTypedValidator, validatorScript)
import           Plutus.V2.Ledger.Api                 (ScriptContext(..), MintingPolicy, TokenName (..), mkMintingPolicyScript,
                                                       ScriptHash, unMintingPolicyScript)
import           PlutusTx                             (compile)
import           PlutusTx.AssocMap                    (fromList)
import           PlutusTx.Prelude                     (BuiltinByteString, Bool (..), ($), map)
import           Plutus.Script.Utils.V2.Scripts       (scriptHash)

------------------------------------- Test Minting Policy --------------------------------------

{-# INLINABLE testTokenName #-}
testTokenName :: BuiltinByteString -> TokenName
testTokenName = TokenName

testPolicyCheck :: [BuiltinByteString] -> ScriptContext -> Bool
testPolicyCheck bss ctx = cond1
  where
    names = map testTokenName bss

    cond1 = tokensMinted ctx $ fromList $ map (, 1) names

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