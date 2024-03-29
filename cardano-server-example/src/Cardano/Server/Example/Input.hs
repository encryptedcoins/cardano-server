{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Cardano.Server.Example.Input where

import           PlutusTx          (unstableMakeIsData)
import           PlutusTx.Builtins (BuiltinByteString, Integer)

newtype TestPolicyInput = TestPolicyInput [(BuiltinByteString, Integer)]
unstableMakeIsData ''TestPolicyInput