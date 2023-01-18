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

import           Cardano.Server.Endpoints.AddSignature (AddSignatureApi, addSignatureHandler)
import           Cardano.Server.Endpoints.Funds        (FundsApi, fundsHandler)
import           Cardano.Server.Endpoints.Tx.Class     (HasTxEndpoints(..))
import           Cardano.Server.Endpoints.Tx.Submit    (SubmitTxApi, submitTxHandler, processQueue)
import           Cardano.Server.Endpoints.Tx.New       (NewTxApi, newTxHandler)
import           Cardano.Server.Endpoints.Ping         (PingApi, pingHandler)
import           Cardano.Server.Error                  (errorMW)
import           Cardano.Server.Internal               (NetworkM(..), Env, loadEnv)
import           Cardano.Server.Tx                     (checkForCleanUtxos)
import           Control.Concurrent                    (forkIO)
import           Control.Monad.Except                  (runExceptT)
import           Control.Monad.Reader                  (ReaderT(runReaderT))
import           Cardano.Server.Utils.Logger           (HasLogger(logMsg))
import qualified Network.Wai                           as Wai
import qualified Network.Wai.Handler.Warp              as Warp
import           Network.Wai.Middleware.Cors           (CorsResourcePolicy(..), simpleCorsResourcePolicy, cors)
import qualified Servant
import           Servant                               (Proxy(..), type (:<|>)(..), ServerT,
                                                        hoistServer, Application, runHandler', serve)
import           System.IO                             (stdout, BufferMode(LineBuffering), hSetBuffering)

type ServerAPI s
    =    PingApi
    :<|> SubmitTxApi s
    :<|> AddSignatureApi s
    :<|> NewTxApi s
    :<|> FundsApi

type ServerConstraints s =
    ( HasTxEndpoints s
    , Servant.HasServer (ServerAPI s) '[]
    )

server :: HasTxEndpoints s => ServerT (ServerAPI s) (NetworkM s)
server = pingHandler
    :<|> submitTxHandler
    :<|> addSignatureHandler
    :<|> newTxHandler
    :<|> fundsHandler

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
        Warp.run port $ errorMW $ mkApp @s env
    where
        prepareServer env = runExceptT . runHandler' . flip runReaderT env . unNetworkM $ do 
            logMsg "Starting server..."
            checkForCleanUtxos

corsWithContentType :: Wai.Middleware
corsWithContentType = cors (const $ Just policy)
    where policy = simpleCorsResourcePolicy { corsRequestHeaders = ["Content-Type"] }

mkApp :: forall s. ServerConstraints s => Env s -> Application
mkApp env = corsWithContentType $ serve (serverAPI @s) $
    hoistServer (serverAPI @s) ((`runReaderT` env) . unNetworkM) server