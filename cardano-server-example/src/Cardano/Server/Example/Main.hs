{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Server.Example.Main
    ( ExampleApi
    , runExampleServer
    ) where

import           Cardano.Server.Error.Class      (IsCardanoServerError (..))
import           Cardano.Server.Example.OffChain (testMintTx)
import           Cardano.Server.Input            (InputContext)
import           Cardano.Server.Internal         (AuxillaryEnvOf, InputOf)
import           Cardano.Server.Main             (ServerApi, runServer)
import           Control.Monad                   (when)
import           Control.Monad.Catch             (Exception, MonadThrow (throwM))
import           Data.List                       (nub)
import           Plutus.V2.Ledger.Api            (BuiltinByteString)
import           PlutusAppsExtra.IO.ChainIndex   (ChainIndex (..))
import           PlutusAppsExtra.IO.Wallet       (getWalletAddr)

type ExampleApi = ServerApi ([BuiltinByteString], InputContext) ExampleApiError

type instance InputOf        ExampleApi = [BuiltinByteString]
type instance AuxillaryEnvOf ExampleApi = ()

data ExampleApiError = HasDuplicates
    deriving (Show, Exception)

instance IsCardanoServerError ExampleApiError where
    errStatus _ = toEnum 422
    errMsg _ = "The request contains duplicate tokens and will not be processed."

runExampleServer :: IO ()
runExampleServer = runServer
        @ExampleApi
        ()
        Kupo
        ((:[]) <$> getWalletAddr)
        (\bbs -> pure [testMintTx bbs])
        (pure ())
        processRequest
    where
        processRequest req@(bbs, _) = do
            let hasDuplicates = length bbs /= length (nub bbs)
            when hasDuplicates $ throwM HasDuplicates
            return req