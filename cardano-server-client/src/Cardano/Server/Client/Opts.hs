{-# LANGUAGE OverloadedStrings   #-}

module Cardano.Server.Client.Opts where

import           Cardano.Server.Client.Internal (Mode (..), ServerEndpoint (..))
import           Control.Applicative            ((<|>))
import           Options.Applicative            (Parser, argument, auto, execParser, fullDesc, help, helper, info, long, metavar,
                                                 option, short, strOption, value, (<**>))

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
    (  value ServerTxE
    <> metavar "ping | funds | newTx | submitTx | serverTx | status"
    )

--------------------------------------------- Auto ---------------------------------------------

autoModeParser :: Parser Mode
autoModeParser = Auto <$> option auto
    (  long  "auto"
    <> short 'a'
    <> help  "Average client request interval in seconds."
    <> value 30
    <> metavar "SECONDS"
    )

-------------------------------------------- Manual --------------------------------------------

manualModeParser :: Parser Mode
manualModeParser = Manual <$> strOption 
    (  short 'm'
    <> long "manual" 
    <> help "Text representation of client argument" 
    <> value "" 
    <> metavar "TEXT"
    )