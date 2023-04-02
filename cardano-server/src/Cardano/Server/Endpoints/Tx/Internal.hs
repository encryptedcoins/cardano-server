{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE EmptyCase         #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}

module Cardano.Server.Endpoints.Tx.Internal where

import           Cardano.Server.Error.Class (IsCardanoServerError (..))
import           Control.Exception          (Exception)
import           Data.Kind                  (Type)

type family TxApiErrorOf api :: Type

data NoError deriving (Show, Exception)

instance IsCardanoServerError NoError where
    errStatus = \case
    errMsg = \case