{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Cardano.Server.Example.Input where

import           Plutus.V2.Ledger.Api (TokenName (..))
import           PlutusTx             (unstableMakeIsData)
import qualified PlutusTx.AssocMap    as AssocMap
import           PlutusTx.Builtins    (Integer)
import           PlutusTx.Prelude     (BuiltinByteString, zipWith, sort)
import PlutusTx.Base (($))

