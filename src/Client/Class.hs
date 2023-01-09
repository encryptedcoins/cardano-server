{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeFamilies               #-}

module Client.Class where

import           Data.Kind            (Type)
import           Options.Applicative  (Parser)
import           Server.Internal      (HasServer(..), AppM)

class ( HasServer c
      , Show (RequestTermOf c)
      , Eq (RequestTermOf c)
      ) => HasClient c where

    type RequestTermOf c :: Type

    parseRequestTerm :: Parser (RequestTermOf c)

    genRequestTerm :: IO (RequestTermOf c)

    -- here ClientM c () are some additional actions, that would be executed
    -- on successful response
    makeServerInput :: ClientRequestOf c -> AppM c (AppM c (), InputOf c)

type ClientRequestOf s = [RequestTermOf s]