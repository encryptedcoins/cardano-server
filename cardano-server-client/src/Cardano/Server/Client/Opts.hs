{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}


module Cardano.Server.Client.Opts where

import           Control.Applicative ((<|>))
import           Options.Applicative (Parser, argument, auto, execParser, flag', fullDesc, help, helper, info, long, metavar,
                                      option, short, strOption, value, (<**>))
import Cardano.Server.Client.Internal ( ServerEndpoint (..), Mode (..), Interval )

runWithOpts :: IO Options
runWithOpts = execParser $ info (optionsParser <**> helper) fullDesc

optionsParser :: Parser Options
optionsParser = Options <$> serverEndpointParser <*> (autoModeParser <|> manualModeParser)

data Options = Options
    { optsEndpoint :: ServerEndpoint
    , optsMode     :: Mode
    } deriving Show

serverEndpointParser :: Parser ServerEndpoint
serverEndpointParser = argument auto
    (  value SubmitTxE
    <> metavar "Ping | Funds | NewTx | SubmitTx | ServerTx"
    )

--------------------------------------------- Auto ---------------------------------------------

autoModeParser :: Parser Mode
autoModeParser
    = flag' Auto (long "auto") <*> intervalParser

intervalParser :: Parser Interval
intervalParser = option auto
    (  long  "interval"
    <> short 'i'
    <> help  "Average client request interval in seconds."
    <> value 30
    <> metavar "SECONDS"
    )

-------------------------------------------- Manual --------------------------------------------

manualModeParser :: Parser Mode
manualModeParser = flag' Manual (long "manual" <> help "Input of manual mode.") 
                  <*> strOption (help "Text representation of client argument" <> value "" <> metavar "TEXT")