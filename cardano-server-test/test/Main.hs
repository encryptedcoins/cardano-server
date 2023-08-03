module Main where

import qualified Cardano.Server.Client.OptsSpec         as ClientOpts
import qualified Cardano.Server.Endpoints.UtxosSpec     as Utxos
import qualified Cardano.Server.Endpoints.PingSpec      as Ping
import qualified Cardano.Server.Endpoints.StatusSpec    as Status
import qualified Cardano.Server.Endpoints.Tx.ServerSpec as ServerTx
import qualified Cardano.Server.Endpoints.Tx.SubmitSpec as SubmitTx
import           Cardano.Server.Example.Main            (exampleServerHandle)
import qualified Cardano.Server.WalletEncryptionSpec    as WalletEncryption
import           Cardano.Server.Test.Internal           (withCardanoServer)

main :: IO ()
main = withCardanoServer "testnet/config.json" exampleServerHandle $ do
    WalletEncryption.spec
    Ping.spec
    Utxos.spec
    ServerTx.spec
    SubmitTx.spec
    Status.spec
    ClientOpts.spec