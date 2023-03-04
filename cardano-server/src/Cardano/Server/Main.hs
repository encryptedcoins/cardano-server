{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Cardano.Server.Main where

import           Cardano.Server.Endpoints.Funds       (FundsApi, fundsHandler)
import           Cardano.Server.Endpoints.Ping        (PingApi, pingHandler)
import           Cardano.Server.Endpoints.Tx.Class    (HasTxEndpoints (..))
import           Cardano.Server.Endpoints.Tx.New      (NewTxApi, newTxHandler)
import           Cardano.Server.Endpoints.Tx.Server   (ServerTxApi, processQueue, serverTxHandler)
import           Cardano.Server.Endpoints.Tx.Submit   (SubmitTxApi, submitTxHandler)
import           Cardano.Server.Error.CommonErrors    (ConnectionError (..))
import           Cardano.Server.Internal              (Env, NetworkM (..), loadEnv)
import           Cardano.Server.Tx                    (checkForCleanUtxos)
import           Cardano.Server.Utils.Logger          (HasLogger (..), logMsg, (.<))
import           Control.Concurrent                   (forkIO)
import           Control.Exception                    (Handler (Handler), catches)
import           Control.Monad                        (void)
import           Control.Monad.Except                 (runExceptT)
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
import           Servant                              (Application, Proxy (..), ServerT, hoistServer, runHandler', serve,
                                                       type (:<|>) (..))
import qualified Servant
import           System.IO                            (BufferMode (LineBuffering), hSetBuffering, stdout)

type ServerAPI s
    =    PingApi
    :<|> FundsApi
    :<|> NewTxApi s
    :<|> SubmitTxApi s
    :<|> ServerTxApi s    

type ServerConstraints s =
    ( HasTxEndpoints s
    , Servant.HasServer (ServerAPI s) '[]
    )

server :: HasTxEndpoints s => ServerT (ServerAPI s) (NetworkM s)
server = pingHandler
    :<|> fundsHandler
    :<|> newTxHandler
    :<|> submitTxHandler
    :<|> serverTxHandler

serverAPI :: forall s. Proxy (ServerAPI s)
serverAPI = Proxy @(ServerAPI s)

port :: Int
port = 3000

runServer :: forall s. ServerConstraints s => IO ()
runServer = (`catches` errorHanlders) $ do
        env <- loadEnv
        hSetBuffering stdout LineBuffering
        forkIO $ processQueue env
        prepareServer env
        Warp.runSettings (settings env) $ mkApp @s env
    where
        unApp env = runExceptT . runHandler' . flip runReaderT env . unNetworkM 
        prepareServer env = unApp env $ do
            logMsg "Starting server..."
            checkForCleanUtxos
        settings env = Warp.setLogger (logReceivedRequest env) 
                     $ Warp.setOnException (const $ logException env)
                     $ Warp.setPort port 
                       Warp.defaultSettings
        logReceivedRequest env req status _ = void . unApp env $
            logMsg $ "Received request:\n" .< req <> "\nStatus:\n" .< status
        logException env e = void . unApp env $
            logMsg $ "Unhandled exception:\n" .< e
        errorHanlders = [Handler connectionErroH]
        connectionErroH e = T.putStrLn $ (<> " is unavailable.") $ case e of
            PlutusChainIndexConnectionError{} -> "Caradno chain index"
            KupoConnectionError{}             -> "Kupo chain index"
            WalletApiConnectionError{}        -> "Cardano wallet"
            ConnectionError req _             -> T.decodeUtf8 $ path req

corsWithContentType :: Wai.Middleware
corsWithContentType = cors (const $ Just policy)
    where policy = simpleCorsResourcePolicy { corsRequestHeaders = ["Content-Type"] }

mkApp :: forall s. ServerConstraints s => Env s -> Application
mkApp env = corsWithContentType $ serve (serverAPI @s) $
    hoistServer (serverAPI @s) ((`runReaderT` env) . unNetworkM) server