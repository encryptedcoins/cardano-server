{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           Cardano.Server.Config               (decodeOrErrorFromFile)
import qualified Cardano.Server.Endpoints.PingSpec   as Ping
import qualified Cardano.Server.Endpoints.StatusSpec as Status
import           Cardano.Server.Example.Main         (ExampleApi, ExampleApiConfig (..), exampleServer, loadExampleApiEnv)
import           Cardano.Server.Test.Internal        (withCardanoServer)
import qualified Cardano.Server.WalletEncryptionSpec as WalletEncryption

main :: IO ()
main = do
    let configFp = "cardano-server-test/test/configuration/config.json"
    ExampleApiConfig{..} <- decodeOrErrorFromFile configFp
    auxEnv <- loadExampleApiEnv ExampleApiConfig{..}
    withCardanoServer @ExampleApi configFp exampleServer (pure ()) auxEnv 0 $ do
        WalletEncryption.spec
        Ping.spec
        Status.spec