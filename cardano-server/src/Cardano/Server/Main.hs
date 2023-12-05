{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImplicitParams        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-orphans       #-}

module Cardano.Server.Main where

import           Cardano.Server.Config                (CardanoServerConfig (..), Config, Creds, HasCreds, HyperTextProtocol (..))
import           Cardano.Server.Diagnostics           (doDiagnostics)
import           Cardano.Server.Endpoints.Ping        (PingApi, pingHandler)
import           Cardano.Server.Endpoints.Status      (StatusApi, commonStatusHandler)
import           Cardano.Server.Endpoints.Tx.Internal (TxApiErrorOf)
import           Cardano.Server.Endpoints.Tx.New      (NewTxApi, newTxHandler)
import           Cardano.Server.Endpoints.Tx.Server   (ServerTxApi, processQueue, serverTxHandler)
import           Cardano.Server.Endpoints.Tx.Submit   (SubmitTxApi, submitTxHandler)
import           Cardano.Server.Endpoints.Utxos       (UtxosApi, utxosHandler)
import           Cardano.Server.Endpoints.Version     (VersionApi, serverVersionHandler)
import           Cardano.Server.Error.Class           (IsCardanoServerError)
import           Cardano.Server.Error.CommonErrors    (ConnectionError (..), logCriticalExceptions)
import           Cardano.Server.Internal              (Env (..), HasStatusEndpoint (..), HasVersionEndpoint (..), InputOf,
                                                       ServerHandle (..), ServerM (..), TxApiRequestOf, loadEnv, runServerM)
import           Cardano.Server.Tx                    (checkForCleanUtxos)
import           Cardano.Server.Utils.Logger          (HasLogger, logMsg, (.<))
import           Cardano.Server.Utils.Wait            (waitTime)
import           Control.Concurrent                   (forkIO)
import           Control.Exception                    (Handler (Handler), catches, throw)
import           Control.Monad.IO.Class               (MonadIO (..))
import           Control.Monad.Reader                 (ReaderT (runReaderT), ask, asks)
import           Data.FileEmbed                       (embedFileIfExists)
import qualified Data.Text.Encoding                   as T
import qualified Data.Text.IO                         as T
import           Network.HTTP.Client                  (path)
import qualified Network.Wai                          as Wai
import qualified Network.Wai.Handler.Warp             as Warp
import           Network.Wai.Handler.WarpTLS          (defaultTlsSettings)
import qualified Network.Wai.Handler.WarpTLS          as Warp
import           Network.Wai.Middleware.Cors          (CorsResourcePolicy (..), cors, simpleCorsResourcePolicy)
import           PlutusAppsExtra.Api.Kupo             (pattern KupoConnectionError)
import           PlutusAppsExtra.IO.ChainIndex.Plutus (pattern PlutusChainIndexConnectionError)
import           PlutusAppsExtra.IO.Wallet            (pattern WalletApiConnectionError)
import           Servant                              (Proxy (..), ServerT, hoistServer, serve, type (:<|>) (..))
import qualified Servant
import           System.IO                            (BufferMode (LineBuffering), hSetBuffering, stdout)

type ServerApi
  txApiReqBody
  txApiError
  statusApiReqBody
  statusApiErrors
  statusApiRes
  versionRes
    = PingApi
    :<|> UtxosApi
    :<|> NewTxApi txApiReqBody txApiError
    :<|> SubmitTxApi txApiError
    :<|> ServerTxApi txApiReqBody txApiError
    :<|> StatusApi statusApiErrors statusApiReqBody statusApiRes
    :<|> VersionApi versionRes

type instance TxApiRequestOf (ServerApi reqBody _ _ _ _ _) = reqBody
type instance TxApiErrorOf (ServerApi _ txApiError _ _ _ _) = txApiError

instance HasStatusEndpoint (ServerApi r e statusReqBody statusErrors statusRes v) where
    type StatusEndpointErrorsOf (ServerApi _ _ _ statusErrors _ _) = statusErrors
    type StatusEndpointReqBodyOf (ServerApi _ _ statusReqBody _ _ _) = statusReqBody
    type StatusEndpointResOf (ServerApi _ _ _ _ statusRes _) = statusRes
    statusHandler reqBody = asks (shStatusHandler . envServerHandle) >>= ($ reqBody)

instance HasVersionEndpoint (ServerApi r e sReq sErr sRes versionRes) where
    type VersionEndpointResOf (ServerApi _ _ _ _ _ versionRes) = versionRes
    versionHandler = do
      r <- ask
      shVersionHandler . envServerHandle $ r

type ServerApi' api
    = ServerApi
    (TxApiRequestOf api)
    (TxApiErrorOf api)
    (StatusEndpointReqBodyOf api)
    (StatusEndpointErrorsOf api)
    (StatusEndpointResOf api)
    (VersionEndpointResOf api)

type ServerConstraints api =
    ( Servant.HasServer (ServerApi' api) '[]
    , IsCardanoServerError (TxApiErrorOf api)
    , Show (InputOf api)
    , Show (TxApiRequestOf api)
    , Show (StatusEndpointReqBodyOf api)
    , Show (VersionEndpointResOf api)
    )

server :: forall api. ServerConstraints api
    => ServerT (ServerApi' api) (ServerM api)
server
    =    pingHandler
    :<|> utxosHandler
    :<|> newTxHandler
    :<|> submitTxHandler
    :<|> serverTxHandler
    :<|> commonStatusHandler
    :<|> serverVersionHandler

serverAPI :: forall api. Proxy (ServerApi' api)
serverAPI = Proxy @(ServerApi' api)

runServer :: (ServerConstraints api, HasCreds) => Config -> ServerHandle api -> IO ()
runServer c sh = (`catches` errorHanlders) $ loadEnv c sh >>= runServer'
    where
        errorHanlders = [Handler connectionErroH]
        connectionErroH e = T.putStrLn $ (<> " is unavailable.") $ case e of
            PlutusChainIndexConnectionError{} -> "Cardano chain index"
            KupoConnectionError{}             -> "Kupo chain index"
            WalletApiConnectionError{}        -> "Cardano wallet"
            ConnectionError req _             -> T.decodeUtf8 $ path req

runServer' :: forall api. (ServerConstraints api, HasCreds) => Env api -> IO ()
runServer' env = runCardanoServer @(ServerApi' api) env runApp server beforeMainLoop
    where
        runApp :: forall a. ServerM api a -> Servant.Handler a
        runApp = (`runReaderT` env) . unServerM
        beforeMainLoop = do
            liftIO $ forkIO $ processQueue env
            logMsg "Starting server..."
            liftIO $ forkIO $ waitTime 10 >> runServerM env doDiagnostics
            checkForCleanUtxos

runCardanoServer :: forall api m c.
    ( CardanoServerConfig c
    , Servant.HasServer api '[]
    , HasLogger m
    , HasCreds
    ) => c -> (forall a. m a -> Servant.Handler a) -> ServerT api m -> m () -> IO ()
runCardanoServer config runApp serverApp beforeMainLoop = do
    hSetBuffering stdout LineBuffering
    case (configHyperTextProtocol config, ?creds) of
        (HTTP, _)                 -> Warp.runSettings settings app
        (HTTPS, Just (cert, key)) -> Warp.runTLS (Warp.tlsSettingsMemory cert key) settings app
        (HTTPS, Nothing)          -> putStrLn noCredsMsg >> Warp.runTLS defaultTlsSettings settings app
    where
        app = corsWithContentType $ serve (Proxy @api) $ hoistServer (Proxy @api) runApp serverApp
        runHandler = fmap (either throw id) . Servant.runHandler
        settings = Warp.setLogger logReceivedRequest
                 $ Warp.setOnException (const logException)
                 $ Warp.setPort (configPort config)
                 $ Warp.setBeforeMainLoop (runHandler $ runApp beforeMainLoop)
                   Warp.defaultSettings
        logReceivedRequest req status _ = runHandler $ runApp $
            logMsg $ "Received request:\n" .< req <> "\nStatus:\n" .< status
        logException = runHandler . runApp . logCriticalExceptions
        noCredsMsg = "No creds given to run with HTTPS. \
                     \If you want to test something on localhost with HTTPS then\
                     \add key.pem and certificate.pem file before compilation. \
                     \If this error doesn't go away, try running `cabal clean` first."

-- Embed https cert and key files on compilation
embedCreds :: Creds
embedCreds =
    let keyCred  = $(embedFileIfExists "../key.pem" )
        certCred = $(embedFileIfExists "../certificate.pem")
    in (,) <$> certCred <*> keyCred

corsWithContentType :: Wai.Middleware
corsWithContentType = cors (const $ Just policy)
    where policy = simpleCorsResourcePolicy
            { corsRequestHeaders = ["Content-Type"]
            }