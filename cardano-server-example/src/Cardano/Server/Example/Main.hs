{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Cardano.Server.Example.Main (ExampleServer, runExampleServer) where

-- import Cardano.Server.Client.Client          (startClient)
import Cardano.Server.Endpoints.Tx.Class     (HasTxEndpoints(..))
import Cardano.Server.Error                  (IsCardanoServerError(..))
import Cardano.Server.Class                  (HasServer(..), InputWithContext)
-- import Cardano.Server.Client.Class           (HasClient(..))
import Cardano.Server.Main                   (runServer)
import Cardano.Server.Example.OffChain (testMintTx)
import Control.Monad                         (when)
import Control.Monad.Catch                   (throwM)
import Data.List                             (nub)
-- import Options.Applicative                   (argument, metavar, str, some)
import Plutus.V2.Ledger.Api                  (BuiltinByteString)
-- import System.Random                         (randomRIO, randomIO)
 
runExampleServer :: IO ()
runExampleServer = runServer @ExampleServer

data ExampleServer

instance HasServer ExampleServer where

    type AuxiliaryEnvOf ExampleServer = ()

    loadAuxiliaryEnv _ = pure ()

    type InputOf ExampleServer = [BuiltinByteString]

instance HasTxEndpoints ExampleServer where

    type TxApiRequestOf ExampleServer = InputWithContext ExampleServer

    data (TxEndpointsErrorOf ExampleServer) = HasDuplicates
        deriving Show

    txEndpointsProcessRequest req@(bbs, _) = do
        let hasDuplicates = length bbs /= length (nub bbs)
        when hasDuplicates $ throwM HasDuplicates
        return req

    txEndpointsTxBuilders bbs = pure [testMintTx bbs]

instance IsCardanoServerError (TxEndpointsErrorOf ExampleServer) where
    errStatus _ = toEnum 422
    errMsg _ = "The request contains duplicate tokens and will not be processed."