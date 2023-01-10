{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeFamilies               #-}

module Client.Class where

import           Options.Applicative  -- (Parser)
import           Server.Internal      (HasServer(..), AppM)
import           System.Random        (Random, randomIO)

class HasServer c => HasClient c where

    -- Input parser for manual client mode.
    parseServerInput :: Parser (InputOf c)
    default parseServerInput :: Read (InputOf c) => Parser (InputOf c)
    parseServerInput = argument auto (metavar "Server input")

    -- Input generator for automatic client mode.
    genServerInput :: AppM c (InputOf c)
    default genServerInput :: Random (InputOf c) => AppM c (InputOf c)
    genServerInput = randomIO

    -- A function that extracts some actions from the input that will be performed before the request is sent 
    -- and after a successful response is received, respectively. 
    -- It can be useful, for example, if you need to write some additional information about your inputs to external files.
    -- Use the default implementation if you don't need either of them.
    extractActionsFromInput :: InputOf c -> AppM c (AppM c (), AppM c ())
    extractActionsFromInput _ = pure (pure (), pure ())