{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module TestingServer.Main (TestingServer) where

import Client.Class                 (HasClient(..))
import Control.Monad                (when, replicateM)
import Control.Monad.Catch          (Exception, throwM)
import Data.List                    (nub)
import Options.Applicative          (argument, metavar, str, some)
import Plutus.V2.Ledger.Api         (BuiltinByteString)
import PlutusTx.Builtins.Class      (stringToBuiltinByteString)
import Server.Endpoints.Tx.Class    (HasTxEndpoints(..))
import Server.Endpoints.Tx.Internal (DefaultTxApiResult)
import Server.Class                 (HasServer(..))
import System.Random                (randomRIO, randomIO)
import TestingServer.OffChain       (testMintTx)
import Utils.Servant                (respondWithStatus)

data TestingServer

instance HasServer TestingServer where

    type AuxiliaryEnvOf TestingServer = ()

    loadAuxiliaryEnv _ = pure ()

    type InputOf TestingServer = [BuiltinByteString]

instance HasTxEndpoints TestingServer where

    type TxApiResultOf TestingServer = DefaultTxApiResult

    data (TxEndpointsErrorOf TestingServer) = HasDuplicates
        deriving (Show, Exception)

    txEndpointsTxBuilders bbs = pure [testMintTx bbs]

    checkForTxEndpointsErrors bbs =
        let hasDuplicates = length bbs /= length (nub bbs)
        in  when hasDuplicates $ throwM HasDuplicates
        
    txEndpointsErrorHandler _ = respondWithStatus @422
        "The request contains duplicate tokens and will not be processed."

instance HasClient TestingServer where

    parseServerInput = some $ stringToBuiltinByteString <$> argument str (metavar "token name")

    genServerInput = do
        inputLength <- randomRIO (1, 15)
        let genBbs = stringToBuiltinByteString <$> (randomRIO (2, 8) >>= (`replicateM` randomIO))
        replicateM inputLength genBbs 