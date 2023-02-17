{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Cardano.Server.Main where

import           Cardano.Server.Endpoints.Funds        (FundsApi, fundsHandler)
import           Cardano.Server.Endpoints.Ping         (PingApi, pingHandler)
import           Cardano.Server.Endpoints.Tx.Class     (HasTxEndpoints (..))
import           Cardano.Server.Endpoints.Tx.New       (NewTxApi, newTxHandler)
import           Cardano.Server.Endpoints.Tx.Server    (ServerTxApi, processQueue, serverTxHandler)
import           Cardano.Server.Endpoints.Tx.Submit    (SubmitTxApi, submitTxHandler)
import           Cardano.Server.Error                  (errorMW)
import           Cardano.Server.Internal               (Env, NetworkM (..), loadEnv)
import           Cardano.Server.Tx                     (checkForCleanUtxos)
import           Cardano.Server.Utils.Logger           (HasLogger (..))
import           Control.Concurrent                    (forkIO)
import           Control.Monad.Except                  (runExceptT)
import           Control.Monad.Reader                  (ReaderT (runReaderT))
import qualified Network.Wai                           as Wai
import qualified Network.Wai.Handler.Warp              as Warp
import           Network.Wai.Middleware.Cors           (CorsResourcePolicy (..), cors, simpleCorsResourcePolicy)
import           Servant                               (Application, Proxy (..), ServerT, hoistServer, runHandler', serve,
                                                        type (:<|>) (..))
import qualified Servant
import           System.IO                             (BufferMode (LineBuffering), hSetBuffering, stdout)

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
runServer = do
        env <- loadEnv
        hSetBuffering stdout LineBuffering
        forkIO $ processQueue env
        prepareServer env
        Warp.runSettings settings
            $ errorMW
            $ mkApp @s env
    where
        prepareServer env = runExceptT . runHandler' . flip runReaderT env . unNetworkM $ do
            logMsg "Starting server..."
            checkForCleanUtxos
        settings = Warp.setPort port Warp.defaultSettings

corsWithContentType :: Wai.Middleware
corsWithContentType = cors (const $ Just policy)
    where policy = simpleCorsResourcePolicy { corsRequestHeaders = ["Content-Type"] }

mkApp :: forall s. ServerConstraints s => Env s -> Application
mkApp env = corsWithContentType $ serve (serverAPI @s) $
    hoistServer (serverAPI @s) ((`runReaderT` env) . unNetworkM) server