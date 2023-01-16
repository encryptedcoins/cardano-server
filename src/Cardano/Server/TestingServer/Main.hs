{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Cardano.Server.TestingServer.Main (TestingServer) where

import Cardano.Server.Endpoints.Tx.Class     (HasTxEndpoints(..))
import Cardano.Server.Endpoints.Tx.Internal  (DefaultTxApiResult)
import Cardano.Server.Endpoints.Servant      (respondWithStatus) 
import Cardano.Server.Class                  (HasServer(..))
import Cardano.Server.Client.Class           (HasClient(..))
import Cardano.Server.TestingServer.OffChain (testMintTx)
import Control.Monad                         (when, replicateM)
import Control.Monad.Catch                   (Exception, throwM)
import Data.List                             (nub)
import Options.Applicative                   (argument, metavar, str, some)
import Plutus.V2.Ledger.Api                  (BuiltinByteString)
import PlutusTx.Builtins.Class               (stringToBuiltinByteString)
import System.Random                         (randomRIO, randomIO)
import Utils.ChainIndex                      (MapUTXO)

data TestingServer

instance HasServer TestingServer where

    type AuxiliaryEnvOf TestingServer = ()

    loadAuxiliaryEnv _ = pure ()

    type InputOf TestingServer = [BuiltinByteString]

instance HasTxEndpoints TestingServer where

    type TxApiRequestOf TestingServer = (InputOf TestingServer, MapUTXO)

    type TxApiResultOf TestingServer = DefaultTxApiResult

    data (TxEndpointsErrorOf TestingServer) = HasDuplicates
        deriving (Show, Exception)

    txEndpointsProcessRequest req@(bbs, _) = do
        let hasDuplicates = length bbs /= length (nub bbs)
        when hasDuplicates $ throwM HasDuplicates
        return req

    txEndpointsTxBuilders bbs = pure [testMintTx bbs]
    
    txEndpointsErrorHandler _ = respondWithStatus @422
        "The request contains duplicate tokens and will not be processed."

instance HasClient TestingServer where

    parseServerInput = some $ stringToBuiltinByteString <$> argument str (metavar "token name")

    genServerInput = do
        inputLength <- randomRIO (1, 15)
        let genBbs = stringToBuiltinByteString <$> (randomRIO (2, 8) >>= (`replicateM` randomIO))
        replicateM inputLength genBbs 