{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}

module Client.Opts where

import           Client.Class          (HasClient(..))
import           Control.Applicative   ((<|>))
import           Options.Applicative   (Parser, (<**>), auto, fullDesc, help, info, long, option, short, value, execParser,
                                        helper, flag', metavar, argument)
import           Server.Class          (HasServer(..))

runWithOpts :: HasClient s => IO (Options s)
runWithOpts = execParser $ info (optionsParser <**> helper) fullDesc

optionsParser :: HasClient s => Parser (Options s)
optionsParser = Options <$> serverEndpointParser <*> (autoModeParser <|> manualModeParser)

data Options s = Options
    { optsEndpoint :: ServerEndpoint
    , optsMode     :: Mode s
    } deriving Show

data ServerEndpoint
    = Ping
    | NewTx
    | SubmitTx
    deriving (Show, Read)

serverEndpointParser :: Parser ServerEndpoint
serverEndpointParser = argument auto 
    (  value SubmitTx
    <> metavar "Ping | SubmitTx | NewTx" 
    )

data Mode s
    = Auto   Interval
    | Manual (InputOf s)
deriving instance HasClient s => Show (Mode s)

--------------------------------------------- Auto ---------------------------------------------

type Interval = Int

autoModeParser :: Parser (Mode s)
autoModeParser
    = flag' Auto (long "auto") <*> intervalParser

intervalParser :: Parser Interval
intervalParser = option auto
    (  long  "interval"
    <> short 'i'
    <> help  "Average client request interval in seconds."
    <> value 30
    )

-------------------------------------------- Manual --------------------------------------------

manualModeParser :: forall s. HasClient s => Parser (Mode s)
manualModeParser = flag' Manual (long "manual") <*> parseServerInput @s