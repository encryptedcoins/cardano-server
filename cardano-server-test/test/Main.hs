{-# LANGUAGE ImplicitParams   #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Cardano.Server.Client.OptsSpec         as ClientOpts
import           Cardano.Server.Config                  (decodeOrErrorFromFile)
import qualified Cardano.Server.Endpoints.PingSpec      as Ping
import qualified Cardano.Server.Endpoints.StatusSpec    as Status
import qualified Cardano.Server.Endpoints.Tx.ServerSpec as ServerTx
import qualified Cardano.Server.Endpoints.Tx.SubmitSpec as SubmitTx
import qualified Cardano.Server.Endpoints.UtxosSpec     as Utxos
import           Cardano.Server.Example.Main            (ExampleApi, exampleServer, exampleServerHandle)
import           Cardano.Server.Internal                (Env (envWallet), loadEnv)
import           Cardano.Server.Main                    (runApp)
import           Cardano.Server.Test.Internal           (withCardanoServer)
import qualified Cardano.Server.WalletEncryptionSpec    as WalletEncryption

main :: IO ()
main = do
    config <- decodeOrErrorFromFile "cardano-server-test/test/configuration/config.json"
    let ?creds = Nothing
    env <- loadEnv config exampleServerHandle
    withCardanoServer @ExampleApi config (runApp env) exampleServer (pure ()) (envWallet env) 1 $ do
        WalletEncryption.spec
        Ping.spec
        Utxos.spec
        ServerTx.spec env
        SubmitTx.spec
        Status.spec
        ClientOpts.spec