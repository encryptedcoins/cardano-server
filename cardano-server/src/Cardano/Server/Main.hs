{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-orphans       #-}

module Cardano.Server.Main where

import           Cardano.Server.Endpoints.Funds       (FundsApi, fundsHandler)
import           Cardano.Server.Endpoints.Ping        (PingApi, pingHandler)
import           Cardano.Server.Endpoints.Status      (StatusApi, commonStatusHandler)
import           Cardano.Server.Endpoints.Tx.Internal (TxApiErrorOf)
import           Cardano.Server.Endpoints.Tx.New      (NewTxApi, newTxHandler)
import           Cardano.Server.Endpoints.Tx.Server   (ServerTxApi, processQueue, serverTxHandler)
import           Cardano.Server.Endpoints.Tx.Submit   (SubmitTxApi, submitTxHandler)
import           Cardano.Server.Error.Class           (IsCardanoServerError)
import           Cardano.Server.Error.CommonErrors    (ConnectionError (..))
import           Cardano.Server.Internal              (Env (..), HasStatusEndpoint (..), InputOf, ServerHandle (..), ServerM (..),
                                                       TxApiRequestOf, envLoggerFilePath, loadEnv, runServerM)
import           Cardano.Server.Tx                    (checkForCleanUtxos)
import           Cardano.Server.Utils.Logger          (logMsg, (.<))
import           Control.Concurrent                   (forkIO)
import           Control.Exception                    (Handler (Handler), catches)
import           Control.Monad.Reader                 (ReaderT (runReaderT), asks)
import qualified Data.Text.Encoding                   as T
import qualified Data.Text.IO                         as T
import           Network.HTTP.Client                  (path)
import qualified Network.Wai                          as Wai
import qualified Network.Wai.Handler.Warp             as Warp
import           Network.Wai.Middleware.Cors          (CorsResourcePolicy (..), cors, simpleCorsResourcePolicy)
import           PlutusAppsExtra.IO.ChainIndex.Kupo   (pattern KupoConnectionError)
import           PlutusAppsExtra.IO.ChainIndex.Plutus (pattern PlutusChainIndexConnectionError)
import           PlutusAppsExtra.IO.Wallet            (pattern WalletApiConnectionError)
import           Servant                              (Application, Proxy (..), ServerT, hoistServer, serve, type (:<|>) (..))
import qualified Servant
import           System.IO                            (BufferMode (LineBuffering), hSetBuffering, stdout)

type ServerApi txApiReqBody txApiError statusApiReqBody statusApiErrors statusApiRes
    = PingApi
    :<|> FundsApi
    :<|> NewTxApi txApiReqBody txApiError
    :<|> SubmitTxApi txApiError
    :<|> ServerTxApi txApiReqBody txApiError
    :<|> StatusApi statusApiErrors statusApiReqBody statusApiRes

type instance TxApiRequestOf (ServerApi reqBody _ _ _ _) = reqBody
type instance TxApiErrorOf (ServerApi _ txApiError _ _ _) = txApiError

instance HasStatusEndpoint (ServerApi r e statusReqBody statusErrors statusRes) where
    type StatusEndpointErrorsOf (ServerApi _ _ _ statusErrors _) = statusErrors
    type StatusEndpointReqBodyOf (ServerApi _ _ statusReqBody _ _) = statusReqBody
    type StatusEndpointResOf (ServerApi _ _ _ _ statusRes) = statusRes
    statusHandler reqBody = asks (shStatusHandler . envServerHandle) >>= ($ reqBody)

type ServerApi' api
    = ServerApi
    (TxApiRequestOf api)
    (TxApiErrorOf api)
    (StatusEndpointReqBodyOf api)
    (StatusEndpointErrorsOf api)
    (StatusEndpointResOf api)

type ServerConstraints api =
    ( Servant.HasServer (ServerApi' api) '[]
    , IsCardanoServerError (TxApiErrorOf api)
    , Show (InputOf api)
    , Show (TxApiRequestOf api)
    , Show (StatusEndpointReqBodyOf api)
    )

server :: forall api. ServerConstraints api
    => ServerT (ServerApi' api) (ServerM api)
server
    =    pingHandler
    :<|> fundsHandler
    :<|> newTxHandler
    :<|> submitTxHandler
    :<|> serverTxHandler
    :<|> commonStatusHandler

serverAPI :: forall api. Proxy (ServerApi' api)
serverAPI = Proxy @(ServerApi' api)

runServer :: ServerConstraints api => ServerHandle api -> IO ()
runServer sh = (`catches` errorHanlders) $ do
        env <- loadEnv sh
        runServer' env
    where
        errorHanlders = [Handler connectionErroH]
        connectionErroH e = T.putStrLn $ (<> " is unavailable.") $ case e of
            PlutusChainIndexConnectionError{} -> "Cardano chain index"
            KupoConnectionError{}             -> "Kupo chain index"
            WalletApiConnectionError{}        -> "Cardano wallet"
            ConnectionError req _             -> T.decodeUtf8 $ path req

runServer' :: ServerConstraints api => Env api -> IO ()
runServer' env = do
        hSetBuffering stdout LineBuffering
        forkIO $ processQueue env
        prepareServer
        Warp.runSettings settings $ mkApp env {envLoggerFilePath = Just "server.log"}
    where
        prepareServer = runServerM env $ do
            logMsg "Starting server..."
            checkForCleanUtxos
        settings = Warp.setLogger logReceivedRequest
                 $ Warp.setOnException (const logException)
                 $ Warp.setPort (envPort env)
                   Warp.defaultSettings
        logReceivedRequest req status _ = runServerM env $
            logMsg $ "Received request:\n" .< req <> "\nStatus:\n" .< status
        logException e = runServerM env $
            logMsg $ "Unhandled exception:\n" .< e

corsWithContentType :: Wai.Middleware
corsWithContentType = cors (const $ Just policy)
    where policy = simpleCorsResourcePolicy { corsRequestHeaders = ["Content-Type"] }

mkApp :: forall api. ServerConstraints api => Env api -> Application
mkApp env 
    = corsWithContentType $ serve (serverAPI @api) $ hoistServer (serverAPI @api) ((`runReaderT` env) . unServerM) server