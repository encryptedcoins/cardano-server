{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}

module Server.Endpoints.Tx.Internal where

import           Data.Aeson                       (ToJSON)
import           Data.Text                        (Text)
import           GHC.Generics                     (Generic)
import           Servant                          (NoContent(..), WithStatus, HasStatus)

type DefaultTxApiResult = 
    '[ WithStatus 422 Text
    ,  WithStatus 503 Text
    ,  NoContent
    ,  NewTxEndpointResult
    ]

newtype NewTxEndpointResult = NewTxEndpointResult Text
    deriving HasStatus via WithStatus 200 NewTxEndpointResult
    deriving (Show, Generic)
    deriving newtype ToJSON 