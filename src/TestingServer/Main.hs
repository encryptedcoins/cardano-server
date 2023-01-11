{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE UndecidableInstances       #-}

module TestingServer.Main (TestingServer) where

import Client.Class                 (HasClient(..))
import Control.Monad                (when, replicateM)
import Control.Monad.Catch          (Exception, throwM)
import Data.List                    (nub)
import Options.Applicative          (argument, metavar, str)
import Plutus.V2.Ledger.Api         (BuiltinByteString)
import PlutusTx.Builtins.Class      (stringToBuiltinByteString)
import Server.Endpoints.Tx.Class    (HasTxEndpoints(..), DefaultTxApiResult)
import Server.Class                 (HasServer(..))
import System.Random                (randomRIO, randomIO)
import TestingServer.OffChain       (testMintTx)
import Utils.Servant                (respondWithStatus)

import Utils.ChainIndex             (MapUTXO)

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

    type RequestTermOf TestingServer = BuiltinByteString

    parseRequestTerm = stringToBuiltinByteString <$> argument str (metavar "token name")

    genRequestTerm = fmap stringToBuiltinByteString $
        randomRIO (2, 8) >>= (`replicateM` randomIO)

    makeServerInput = pure . (pure (),)