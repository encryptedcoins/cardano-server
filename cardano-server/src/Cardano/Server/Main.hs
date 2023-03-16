{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Cardano.Server.Main where

import           Cardano.Server.Endpoints.Funds       (FundsApi, fundsHandler)
import           Cardano.Server.Endpoints.Ping        (PingApi, pingHandler)
import           Cardano.Server.Endpoints.Tx.Internal (TxApiErrorOf)
import           Cardano.Server.Endpoints.Tx.New      (NewTxApi, newTxHandler)
import           Cardano.Server.Endpoints.Tx.Server   (ServerTxApi, processQueue, serverTxHandler)
import           Cardano.Server.Endpoints.Tx.Submit   (SubmitTxApi, submitTxHandler)
import           Cardano.Server.Error.Class           (IsCardanoServerError)
import           Cardano.Server.Error.CommonErrors    (ConnectionError (..))
import           Cardano.Server.Internal              (Env (..), InputOf, InputWithContext, ServerHandle (..), ServerM (..),
                                                       TxApiRequestOf, envLoggerFilePath, loadEnv, runServerM)
import           Cardano.Server.Tx                    (checkForCleanUtxos)
import           Cardano.Server.Utils.Logger          (HasLogger (..), logMsg, (.<))
import           Control.Concurrent                   (forkIO)
import           Control.Exception                    (Handler (Handler), catches)
import           Control.Monad.Reader                 (ReaderT (runReaderT))
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

type ServerApi reqBody txApiError
    = PingApi
    :<|> FundsApi
    :<|> NewTxApi reqBody txApiError
    :<|> SubmitTxApi txApiError
    :<|> ServerTxApi reqBody txApiError

type instance TxApiRequestOf (ServerApi reqBody _) = reqBody
type instance TxApiErrorOf (ServerApi _ txApiError) = txApiError

type ServerApi' api = ServerApi (TxApiRequestOf api) (TxApiErrorOf api)

type ServerConstraints api =
    ( Servant.HasServer (ServerApi' api) '[]
    , IsCardanoServerError (TxApiErrorOf api)
    , Show (InputOf api)
    , Show (TxApiRequestOf api)
    )

server :: ServerConstraints api => (TxApiRequestOf api -> ServerM api (InputWithContext api)) -> ServerT (ServerApi' api) (ServerM api)
server processRequest
    =    pingHandler
    :<|> fundsHandler
    :<|> newTxHandler processRequest
    :<|> submitTxHandler
    :<|> serverTxHandler processRequest

serverAPI :: forall api. Proxy (ServerApi' api)
serverAPI = Proxy @(ServerApi' api)

port :: Int
port = 3000

runServer :: ServerConstraints api
    => ServerHandle api
    -> IO ()
runServer sh = (`catches` errorHanlders) $ do
        env <- loadEnv sh
        hSetBuffering stdout LineBuffering
        forkIO $ processQueue env
        prepareServer env
        Warp.runSettings (settings env) $ mkApp env {envLoggerFilePath = Just "server.log"}
    where
        prepareServer env = runServerM env $ do
            logMsg "Starting server..."
            checkForCleanUtxos
        settings env = Warp.setLogger (logReceivedRequest env)
                     $ Warp.setOnException (const $ logException env)
                     $ Warp.setPort port
                       Warp.defaultSettings
        logReceivedRequest env req status _ = runServerM env $
            logMsg $ "Received request:\n" .< req <> "\nStatus:\n" .< status
        logException env e = runServerM env $
            logMsg $ "Unhandled exception:\n" .< e
        errorHanlders = [Handler connectionErroH]
        connectionErroH e = T.putStrLn $ (<> " is unavailable.") $ case e of
            PlutusChainIndexConnectionError{} -> "Cardano chain index"
            KupoConnectionError{}             -> "Kupo chain index"
            WalletApiConnectionError{}        -> "Cardano wallet"
            ConnectionError req _             -> T.decodeUtf8 $ path req

corsWithContentType :: Wai.Middleware
corsWithContentType = cors (const $ Just policy)
    where policy = simpleCorsResourcePolicy { corsRequestHeaders = ["Content-Type"] }

mkApp :: forall api. ServerConstraints api => Env api -> Application
mkApp env = corsWithContentType $ serve (serverAPI @api) $
    hoistServer (serverAPI @api) ((`runReaderT` env) . unServerM) $ server (shProcessRequest $ envServerHandle env)