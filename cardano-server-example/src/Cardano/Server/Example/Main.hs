{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Server.Example.Main where

import           Cardano.Server.Error            (Envelope, toEnvelope)
import           Cardano.Server.Error.Class      (IsCardanoServerError (..))
import           Cardano.Server.Example.OffChain (testMintTx)
import           Cardano.Server.Input            (InputContext)
import           Cardano.Server.Internal         (AuxillaryEnvOf, InputOf, ServerHandle (ServerHandle), ServerM)
import           Cardano.Server.Main             (ServerApi, runServer)
import           Control.Monad                   (when)
import           Control.Monad.Catch             (Exception, MonadThrow (throwM))
import           Data.List                       (nub, sort)
import           Data.Text                       (Text)
import           Plutus.V2.Ledger.Api            (BuiltinByteString)
import           PlutusAppsExtra.IO.ChainIndex   (ChainIndex (..))
import           PlutusAppsExtra.IO.Wallet       (getWalletAddr)

type ExampleApi 
    = ServerApi 
    ([BuiltinByteString], InputContext) -- Request body of tx endpoints
    ExampleApiError                     -- Error of tx endpoints
    Bool                                -- RequestBody of status enpoint
    '[ExampleStatusEndpointError]       -- Errors of status endpoint
    Text                                -- Result of status endpoint

type instance InputOf        ExampleApi = [BuiltinByteString]
type instance AuxillaryEnvOf ExampleApi = ()

data ExampleApiError = HasDuplicates
    deriving (Show, Exception)

instance IsCardanoServerError ExampleApiError where
    errStatus _ = toEnum 422
    errMsg _ = "The request contains duplicate tokens and will not be processed."

exampleHandle :: ServerHandle ExampleApi
exampleHandle = ServerHandle
        Kupo
        ()
        ((:[]) <$> getWalletAddr)
        (\bbs -> pure [testMintTx bbs])
        (pure ())
        processRequest
        statusEndpointHandler
    where
        processRequest (bbs, ctx) = do
            let hasDuplicates = length bbs /= length (nub bbs)
            when hasDuplicates $ throwM HasDuplicates
            return (sort bbs, ctx)

runExampleServer :: IO ()
runExampleServer = runServer exampleHandle
    
data ExampleStatusEndpointError = ExampleStatusEndpointError
    deriving (Show, Exception)

instance IsCardanoServerError ExampleStatusEndpointError where
    errStatus _ = toEnum 422
    errMsg _ = "This is an example of an error in the status endpoint."

statusEndpointHandler :: Bool -> ServerM ExampleApi (Envelope '[ExampleStatusEndpointError] Text)
statusEndpointHandler b = toEnvelope $ 
    if b 
    then pure "This is an example of a status endpoint." 
    else throwM ExampleStatusEndpointError