{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Cardano.Server.Client.Opts where

import           Cardano.Server.Client.Internal (Interval, Mode (..), ServerEndpoint (..))
import           Control.Applicative            ((<|>))
import           Options.Applicative            (Parser, argument, auto, execParser, flag', fullDesc, help, helper, info, long,
                                                 metavar, option, short, strOption, value, (<**>))

runWithOpts :: IO Options
runWithOpts = execParser $ info (optionsParser <**> helper) fullDesc

optionsParser :: Parser Options
optionsParser = Options <$> serverEndpointParser <*> (autoModeParser <|> manualModeParser)

data Options = Options
    { optsEndpoint :: ServerEndpoint
    , optsMode     :: Mode
    } deriving (Show, Eq)

serverEndpointParser :: Parser ServerEndpoint
serverEndpointParser = argument auto
    (  value SubmitTxE
    <> metavar "ping | funds | newTx | submitTx | serverTx | status"
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
                  <*> strOption (short 'i' <> help "Text representation of client argument" <> value "" <> metavar "TEXT")