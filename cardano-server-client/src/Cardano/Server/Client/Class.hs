{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

module Cardano.Server.Client.Class where

import           Cardano.Server.Class    (InputWithContext)
import           Cardano.Server.Internal (HasServer(..), AppM)
import           Data.Aeson              (ToJSON)
import           Data.Default            (def)
import           Data.Kind               (Type)
import           Options.Applicative     (auto, ReadM)
import           System.Random           (Random, randomIO)

class (HasServer c
      , ToJSON (ClientInput c)
      , Show (ClientInput c)
      ) => HasClient c where

    type ClientInput c :: Type

    -- Input parser for manual client mode.
    parseClientInput :: ReadM (ClientInput c)
    default parseClientInput :: Read (ClientInput c) => ReadM (ClientInput c)
    parseClientInput = auto

    -- Input generator for automatic client mode.
    genClientInput :: AppM c (ClientInput c)
    default genClientInput :: Random (ClientInput c) => AppM c (ClientInput c)
    genClientInput = randomIO

    -- A function that extracts some actions from the input that will be performed before the request is sent 
    -- and after a successful response is received, respectively. 
    -- It can be useful, for example, if you need to write some additional information about your inputs to external files.
    -- Use the default implementation if you don't need either of them.
    extractActionsFromInput :: ClientInput c -> AppM c (AppM c (), AppM c ())
    extractActionsFromInput _ = pure (pure (), pure ())

    -- Converts a client input into the server input
    toServerInput :: ClientInput c -> AppM c (InputOf c)

    -- Function that adds InputContext to the request
    addInputContext :: InputOf c -> AppM c (InputWithContext c)
    addInputContext = pure . (, def)