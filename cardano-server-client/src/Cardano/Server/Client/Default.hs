{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

module Cardano.Server.Client.Default where

import           Cardano.Server.Client.Client (mkRequest)
import           Cardano.Server.Config        (Config(..), loadConfig, decodeOrErrorFromFile)
import           Cardano.Server.Input         (InputContext(..))
import           Cardano.Server.Internal      (runAppM, HasServer(..))
import           Control.Monad                (void)
import           Data.Aeson                   (FromJSON)
import           Data.Default                 (def)
import qualified Data.Text                    as T
import           Network.HTTP.Client          (defaultManagerSettings, newManager)
import           Options.Applicative          ((<**>), auto, fullDesc, help, info, long, option, short, value,
                                               execParser, helper)

-- Running a client that only needs fromJSON instance of server input
-- instead of defining a full HasClient class.
runDefaultClient :: forall s. (HasServer s, FromJSON (InputOf s)) => IO ()
runDefaultClient = getArgs >>= defaultClient @s
    where
        getArgs = execParser $ info (filePathArg <**> helper) fullDesc
        filePathArg = option auto 
            (  long  "filepath"
            <> short 'f'
            <> help  "JSON file with server input."
            <> value "testnet/defaultClientInput.json"
            )

defaultClient :: forall s. (HasServer s, FromJSON (InputOf s)) => FilePath -> IO ()
defaultClient fp = void $ do
    serverInput <- decodeOrErrorFromFile @(InputOf s) fp
    Config{..}  <- loadConfig 
    manager     <- newManager defaultManagerSettings
    let fullAddress = concat 
            ["http://", T.unpack cServerAddress, "/serverTx"]
    runAppM @s $ mkRequest fullAddress manager (serverInput, def :: InputContext)