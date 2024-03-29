{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ImplicitParams       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Server.Example.Main where

import           Cardano.Server.Client.Internal       (statusC)
import           Cardano.Server.Config                (decodeOrErrorFromFile)
import           Cardano.Server.Endpoints.Ping        (PingApi, pingHandler)
import           Cardano.Server.Endpoints.Status      (StatusApi)
import           Cardano.Server.Endpoints.Tx.Internal (TxApiErrorOf)
import           Cardano.Server.Endpoints.Tx.New      (NewTxApi, newTxHandler)
import           Cardano.Server.Endpoints.Tx.Server   (ServerTxApi)
import           Cardano.Server.Endpoints.Tx.Submit   (SubmitTxApi, submitTxHandler)
import           Cardano.Server.Endpoints.Utxos       (UtxosApi, utxosHandler)
import           Cardano.Server.Endpoints.Version     (VersionApi)
import           Cardano.Server.Error                 (ConnectionError (..), Envelope, IsCardanoServerError (..), toEnvelope)
import           Cardano.Server.Example.OffChain      (testMintTx)
import           Cardano.Server.Input                 (InputContext (..))
import           Cardano.Server.Internal              (AuxillaryEnvOf, HasStatusEndpoint (..), HasVersionEndpoint (..), InputOf,
                                                       ServerHandle (ServerHandle), ServerM, TxApiRequestOf, loadEnv, mkServerClientEnv, InputWithContext)
import           Cardano.Server.Main                  (embedCreds, runServer)
import           Cardano.Server.Tx                    (mkTx)
import           Cardano.Server.Utils.Logger          (logMsg, (.<))
import           Control.Monad                        (when)
import           Control.Monad.Catch                  (Exception, MonadThrow (throwM))
import           Control.Monad.IO.Class               (MonadIO (..))
import           Data.Functor                         (($>))
import           Data.List                            (nub, sort)
import           Data.Text                            (Text)
import           Plutus.V2.Ledger.Api                 (BuiltinByteString)
import           PlutusAppsExtra.IO.ChainIndex        (ChainIndexProvider (..))
import           PlutusAppsExtra.IO.Wallet            (getWalletAddr)
import           Servant                              (Get, HasServer (ServerT), JSON, NoContent (..), type (:<|>) (..), type (:>))
import           Servant.Client                       (runClientM)

exampleServer :: Servant.ServerT ExampleApi (ServerM ExampleApi)
exampleServer
    =    pingHandler
    :<|> versionEndpointHandler
    :<|> statusHandler
    :<|> serverTxHandler
    :<|> utxosHandler
    :<|> newTxHandler
    :<|> submitTxHandler

type ExampleApi
    =    PingApi
    :<|> VersionApi Text
    :<|> StatusApi '[ExampleStatusEndpointError] Bool Text
    :<|> ServerTxApi ([(BuiltinByteString, Integer)], InputContext) ExampleApiError
    :<|> UtxosApi
    :<|> NewTxApi  ([(BuiltinByteString, Integer)], InputContext) ExampleApiError
    :<|> SubmitTxApi ExampleApiError

type instance AuxillaryEnvOf ExampleApi = ()

exampleServerHandle :: ServerHandle ExampleApi
exampleServerHandle = ServerHandle
    Kupo                            -- Default chain index
    ()                              -- Server auxillary env
    ((:[]) <$> getWalletAddr)       -- How to get server tracked addresses
    buildTx                         -- How to build tx that will handle server input
    (pure ())                       -- What should the server do when there are no requests
    processRequest                  -- How to extract input from request in tx endpoints
    statusHandler                   -- Handler of status endpoint
    checkStatusEndpoint             -- How to check if status endpoint is alive.
    versionEndpointHandler          -- Handler of version endpoint
  where
    buildTx bbs = pure [testMintTx bbs]

runExampleServer :: FilePath -> IO ()
runExampleServer configFp = do
    config <- decodeOrErrorFromFile configFp
    let ?creds    = embedCreds
    env <- loadEnv config exampleServerHandle
    runServer env exampleServer (pure ())

--------------------------------------------
-- | * ServerTx endpoint
--------------------------------------------

type instance InputOf        ExampleApi = [(BuiltinByteString, Integer)]
type instance TxApiRequestOf ExampleApi = InputWithContext ExampleApi
type instance TxApiErrorOf   ExampleApi = ExampleApiError

data ExampleApiError = HasDuplicates
    deriving (Show, Exception)

instance IsCardanoServerError ExampleApiError where
    errStatus _ = toEnum 422
    errMsg _ = "The request contains duplicate tokens and will not be processed."

processRequest :: ([(BuiltinByteString, Integer)], InputContext)
    -> ServerM ExampleApi ([(BuiltinByteString, Integer)], InputContext)
processRequest (bbsWithAmt, ctx) = do
    let hasDuplicates = length bbsWithAmt /= length (nub $ fst <$> bbsWithAmt)
    when hasDuplicates $ throwM HasDuplicates
    return (sort bbsWithAmt, ctx)

serverTxHandler :: ([(BuiltinByteString, Integer)], InputContext)
    -> ServerM ExampleApi (Envelope '[ExampleApiError, ConnectionError] NoContent)
serverTxHandler arg = toEnvelope $ ($> NoContent) $ do
    logMsg $ "Recieved mint request:\n" .< arg
    (bbsWithAmt, ctx) <- processRequest  arg
    addr <- getWalletAddr
    mkTx [addr] ctx [testMintTx bbsWithAmt] Nothing

--------------------------------------------
-- | * Status endpoint
--------------------------------------------

data ExampleStatusEndpointError = ExampleStatusEndpointError
    deriving (Show, Exception)

instance IsCardanoServerError ExampleStatusEndpointError where
    errStatus _ = toEnum 422
    errMsg _ = "This is an example of an error in the status endpoint."

instance HasStatusEndpoint ExampleApi where
    type StatusEndpointReqBodyOf ExampleApi = Bool
    type StatusEndpointErrorsOf ExampleApi = '[ExampleStatusEndpointError]
    type StatusEndpointResOf ExampleApi = Text
    statusHandler b = toEnvelope $
        if b
        then pure "This is an example of a status endpoint."
        else throwM ExampleStatusEndpointError

checkStatusEndpoint :: ServerM ExampleApi (Either Text ())
checkStatusEndpoint = do
    env <- mkServerClientEnv
    res <- liftIO $ runClientM (statusC @ExampleApi True) env
    either throwM (const $ pure $ Right ()) res

--------------------------------------------
-- | * Version endpoint
--------------------------------------------

instance HasVersionEndpoint ExampleApi where
    type VersionEndpointResOf ExampleApi = Text
    versionHandler = versionEndpointHandler

type VersionEndpoint =  "version" :> Get '[JSON] NoContent

versionEndpointHandler :: ServerM ExampleApi Text
versionEndpointHandler =
    pure "This is an example of a version endpoint."