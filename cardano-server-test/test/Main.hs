{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ImplicitParams      #-}

module Main where

import           Cardano.Server.Client.Client           (createServantClientEnv)
import qualified Cardano.Server.Endpoints.FundsSpec     as Funds
import qualified Cardano.Server.Endpoints.PingSpec      as Ping
import qualified Cardano.Server.Endpoints.StatusSpec    as Status
import qualified Cardano.Server.Endpoints.Tx.ServerSpec as ServerTx
import qualified Cardano.Server.Endpoints.Tx.SubmitSpec as SubmitTx
import           Cardano.Server.Example.Main            (exampleHandle)
import           Cardano.Server.Internal                (envLogger, loadEnv)
import           Cardano.Server.Main                    (runServer')
import           Cardano.Server.Utils.Logger            (mutedLogger)
import           Cardano.Server.Utils.Wait              (waitTime)
import qualified Control.Concurrent                     as C
import           Control.Exception                      (bracket)
import           Control.Monad.IO.Class                 (MonadIO (..))
import           Test.Hspec                             (hspec)

main :: IO ()
main =  do
    env <- loadEnv exampleHandle
    sce <- createServantClientEnv
    let ?servantClientEnv = sce
    bracket (liftIO $ C.forkIO $ runServer' env{envLogger = mutedLogger}) 
        C.killThread $ 
        const $ (waitTime 5 >>) $ hspec $ do
            Ping.spec
            Funds.spec
            ServerTx.spec
            SubmitTx.spec
            Status.spec

