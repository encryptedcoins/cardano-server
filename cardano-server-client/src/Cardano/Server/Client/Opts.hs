{-# LANGUAGE OverloadedStrings #-}

module Cardano.Server.Client.Opts where

import           Cardano.Server.Client.Internal (Mode (..))
import           Control.Applicative            ((<|>))
import           Control.Monad.Reader           (ask)
import           Data.Functor                   ((<&>))
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Options.Applicative            (Parser, argument, auto, execParser, fullDesc, help, helper, info, long, metavar, option,
                                                 short, strOption, value, (<**>))
import           Options.Applicative.Types      (ReadM (..))

runWithOpts :: IO CommonOptions
runWithOpts = execParser $ info (optionsParser <**> helper) fullDesc

optionsParser :: Parser CommonOptions
optionsParser = CommonOptions <$> serverEndpointParser <*> (autoModeParser <|> manualModeParser)

data CommonOptions = CommonOptions
    { optsEndpoint :: Text
    , optsMode     :: Mode
    } deriving (Show, Eq)

serverEndpointParser :: Parser Text
serverEndpointParser = argument (ReadM $ ask <&> T.pack)
    (  value "serverTx"
    <> metavar "ping | utxos | newTx | submitTx | serverTx | status"
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