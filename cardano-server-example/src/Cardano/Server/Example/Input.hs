{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Cardano.Server.Example.Input where

import           Plutus.V2.Ledger.Api (TokenName (..))
import           PlutusTx             (unstableMakeIsData)
import qualified PlutusTx.AssocMap    as AssocMap
import           PlutusTx.Builtins    (Integer)
import           PlutusTx.Prelude     (BuiltinByteString, zipWith)

data TestPolicyInput = TestPolicyInput [BuiltinByteString] [Integer]
unstableMakeIsData ''TestPolicyInput

{-# INLINABLE toTokenMap #-}
toTokenMap :: TestPolicyInput -> AssocMap.Map TokenName Integer
toTokenMap (TestPolicyInput bbss amts) = AssocMap.fromList (zipWith (\bbs amt -> (TokenName bbs, amt)) bbss amts)