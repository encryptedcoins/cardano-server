{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ImplicitParams       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Server.Example.Main where

import           Cardano.Server.Config                (decodeOrErrorFromFile)
import           Cardano.Server.Diagnostics           (doDiagnostics, pingDiagnostics, providersDiagnostics)
import           Cardano.Server.Endpoints.Ping        (PingApi, pingHandler)
import           Cardano.Server.Endpoints.Tx.Internal (TxApiErrorOf)
import           Cardano.Server.Endpoints.Tx.New      (NewTxApi, newTxHandler)
import           Cardano.Server.Endpoints.Tx.Server   (ServerTxApi)
import           Cardano.Server.Endpoints.Tx.Submit   (SubmitTxApi, submitTxHandler)
import           Cardano.Server.Endpoints.Utxos       (UtxosApi, utxosHandler)
import           Cardano.Server.Endpoints.Version     (VersionApi, versionEndpointHandler)
import           Cardano.Server.Error                 (ConnectionError (..), Envelope, IsCardanoServerError (..), Throws, toEnvelope)
import           Cardano.Server.Example.OffChain      (testMintTx)
import           Cardano.Server.Input                 (InputContext (..))
import           Cardano.Server.Internal              (AuxillaryEnvOf, InputOf, InputWithContext, ServerHandle (ServerHandle), ServerM,
                                                       TxApiRequestOf, envDiagnosticsInterval, loadEnv)
import           Cardano.Server.Main                  (embedCreds, runServer)
import           Cardano.Server.Tx                    (mkTx)
import           Cardano.Server.Utils.Logger          (logMsg, (.<))
import           Control.Monad                        (when)
import           Control.Monad.Catch                  (Exception, MonadThrow (throwM))
import           Data.Functor                         (($>))
import           Data.List                            (nub, sort)
import           Data.Text                            (Text)
import           Development.GitRev                   (gitCommitDate, gitHash)
import           Encoins.Common.Version               (AppVersion)
import           Paths_cardano_server_example         (version)
import           Plutus.V2.Ledger.Api                 (BuiltinByteString)
import           PlutusAppsExtra.IO.ChainIndex        (ChainIndexProvider (..))
import           PlutusAppsExtra.IO.Wallet            (getWalletAddr)
import           Servant                              (HasServer (ServerT), JSON, NoContent (..), Post, ReqBody, type (:<|>) (..),
                                                       type (:>))

exampleServer :: Servant.ServerT ExampleApi (ServerM ExampleApi)
exampleServer
    =    pingHandler
    :<|> versionEndpointHandler version $gitHash $gitCommitDate
    :<|> statusHandler
    :<|> serverTxHandler
    :<|> utxosHandler
    :<|> newTxHandler
    :<|> submitTxHandler

type ExampleApi
    =    PingApi
    :<|> VersionApi
    :<|> StatusApi
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
    processRequest                  -- How to extract input from request in tx endpoints
  where
    buildTx bbs = pure [testMintTx bbs]

runExampleServer :: FilePath -> IO ()
runExampleServer configFp = do
    config <- decodeOrErrorFromFile configFp
    let ?creds = embedCreds
    env <- loadEnv config exampleServerHandle
    runServer env exampleServer (serverIdle $ envDiagnosticsInterval env)

serverIdle :: Int -> ServerM ExampleApi ()
serverIdle i = doDiagnostics i $ do
    providersDiagnostics
    pingDiagnostics

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

type StatusApi = "status"
    :> Throws ExampleStatusEndpointError
    :> ReqBody '[JSON] Bool
    :> Post '[JSON] Text

data ExampleStatusEndpointError = ExampleStatusEndpointError
    deriving (Show, Exception)

instance IsCardanoServerError ExampleStatusEndpointError where
    errStatus _ = toEnum 422
    errMsg _ = "This is an example of an error in the status endpoint."

statusHandler :: Bool -> ServerM ExampleApi (Envelope '[ExampleStatusEndpointError] Text)
statusHandler b = toEnvelope $
    if b
    then pure "This is an example of a status endpoint."
    else throwM ExampleStatusEndpointError

--------------------------------------------
-- | * Version endpoint
--------------------------------------------

cardanoServerExampleVersion :: ServerM ExampleApi AppVersion
cardanoServerExampleVersion = versionEndpointHandler version $gitHash $gitCommitDate