{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

module Client.Default where

import           Client.Client             (mkRequest)
import           Data.Aeson                (FromJSON)
import qualified Data.Text                 as T
import           Network.HTTP.Client       (defaultManagerSettings, newManager)
import           Server.Internal           (runAppM, HasServer(..))
import           Server.Config             (Config(..), loadConfig, decodeOrErrorFromFile)       
import           Utils.Logger              (HasLogger(..), (.<))
import           Options.Applicative       ((<**>), auto, fullDesc, help, info, long, option, short, value, execParser, helper)

-- Running a client that only needs fromJSON instance of server input
-- instead of defining a full HasClient class.
defaultClient :: forall s. (HasServer s, FromJSON (InputOf s)) => IO ()
defaultClient = do
        fp          <- getArgs
        serverInput <- decodeOrErrorFromFile @(InputOf s) fp
        Config{..}  <- loadConfig 
        manager     <- newManager defaultManagerSettings
        let fullAddress = concat 
                ["http://", T.unpack cServerAddress, "/relayRequestSumbitTx"]
        runAppM @s $ do
            logMsg $ "New input to send:\n" .< serverInput
            resp <- mkRequest fullAddress manager serverInput
            logMsg $ "Received response:" .< resp
    where
        getArgs = execParser $ info (filePathArg <**> helper) fullDesc
        filePathArg = option auto 
            (  long  "filepath"
            <> short 'f'
            <> help  "JSON file with server input."
            <> value "testnet/defaultClientInput.json"
            )