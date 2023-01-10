{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}

module Client.Opts where

import           Client.Class          (HasClient(..))
import           Control.Applicative   (some, (<|>))
import           Options.Applicative   (Parser, (<**>), auto, fullDesc, help, info, long, option, short, value, execParser,
                                        helper, flag', metavar, argument)

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
    = Auto   AutoOptions
    | Manual [RequestTermOf s]
deriving instance HasClient s => Show (Mode s)

--------------------------------------------- Auto ---------------------------------------------

data AutoOptions = AutoOptions
    { averageRequestInterval :: Interval
    , maxTokensInReq         :: Maximum
    } deriving Show

type Interval = Int
type Maximum  = Int

autoModeParser :: Parser (Mode s)
autoModeParser
    = fmap Auto $ flag' AutoOptions (long "auto")
    <*> intervalParser
    <*> maxTokensParser

intervalParser :: Parser Interval
intervalParser = option auto
    (  long  "interval"
    <> short 'i'
    <> help  "Average client request interval in seconds."
    <> value 30
    )

maxTokensParser :: Parser Maximum
maxTokensParser = option auto
    (  long  "max"
    <> short 'm'
    <> help  "Upper bound on the number of generated tokens in a single request."
    <> value 1
    )

-------------------------------------------- Manual --------------------------------------------

manualModeParser :: forall s. HasClient s => Parser (Mode s)
manualModeParser = flag' Manual (long "manual")
               <*> some (parseRequestTerm @s)