{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

module Cardano.Server.Client.Class where

import           Cardano.Server.Class              (InputWithContext)
import           Cardano.Server.Endpoints.Tx.Class (HasTxEndpoints (..))
import           Cardano.Server.Internal           (AppM, HasServer (..))
import           Data.Aeson                        (ToJSON)
import           Data.Default                      (def)
import           Data.Kind                         (Type)
import           Options.Applicative               (Parser, auto, help, long, option)
import           Servant                           (JSON, MimeRender)
import           System.Random                     (Random, randomIO)

class ( HasServer c
      , ToJSON (ClientInput c)
      , Show (ClientInput c)
      , MimeRender JSON (TxApiRequestOf c)
      ) => HasClient c where

    type ClientInput c :: Type

    -- Input parser for manual client mode.
    parseClientInput :: Parser (ClientInput c)
    default parseClientInput :: Read (ClientInput c) => Parser (ClientInput c)
    parseClientInput = option auto (long "input" <> help "client input")

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
    default toServerInput :: ClientInput c ~ InputOf c => ClientInput c -> AppM c (InputOf c)
    toServerInput = pure

    -- Function that adds InputContext to the request
    addInputContext :: InputOf c -> AppM c (InputWithContext c)
    addInputContext = pure . (, def)