{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Cardano.Server.TestingServer.Main (TestingServer, runTestingServer, runTestingClient) where

import Cardano.Server.Client.Client          (startClient)
import Cardano.Server.Endpoints.Tx.Class     (HasTxEndpoints(..))
import Cardano.Server.Error                  (IsCardanoServerError(..))
import Cardano.Server.Class                  (HasServer(..), InputWithContext)
import Cardano.Server.Client.Class           (HasClient(..))
import Cardano.Server.Main                   (runServer)
import Cardano.Server.TestingServer.OffChain (testMintTx)
import Control.Monad                         (when, replicateM)
import Control.Monad.Catch                   (throwM)
import Data.List                             (nub)
import Options.Applicative                   (argument, metavar, str, some)
import Plutus.V2.Ledger.Api                  (BuiltinByteString)
import PlutusTx.Builtins.Class               (stringToBuiltinByteString)
import System.Random                         (randomRIO, randomIO)
 
runTestingServer :: IO ()
runTestingServer = runServer @TestingServer

runTestingClient :: IO ()
runTestingClient = startClient @TestingServer

data TestingServer

instance HasServer TestingServer where

    type AuxiliaryEnvOf TestingServer = ()

    loadAuxiliaryEnv _ = pure ()

    type InputOf TestingServer = [BuiltinByteString]

instance HasTxEndpoints TestingServer where

    type TxApiRequestOf TestingServer = InputWithContext TestingServer

    data (TxEndpointsErrorOf TestingServer) = HasDuplicates
        deriving Show

    txEndpointsProcessRequest req@(bbs, _) = do
        let hasDuplicates = length bbs /= length (nub bbs)
        when hasDuplicates $ throwM HasDuplicates
        return req

    txEndpointsTxBuilders bbs = pure [testMintTx bbs]

instance IsCardanoServerError (TxEndpointsErrorOf TestingServer) where
    errStatus _ = toEnum 422
    errMsg _ = "The request contains duplicate tokens and will not be processed."

instance HasClient TestingServer where

    parseServerInput = some $ stringToBuiltinByteString <$> argument str (metavar "token name")

    genServerInput = do
        inputLength <- randomRIO (1, 15)
        let genBbs = stringToBuiltinByteString <$> (randomRIO (2, 8) >>= (`replicateM` randomIO))
        replicateM inputLength genBbs