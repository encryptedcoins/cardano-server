{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Server.Main where

import           Control.Concurrent       (forkIO)
import           Control.Monad.Except     (runExceptT)
import           Control.Monad.Reader     (ReaderT(runReaderT))
import           Utils.Logger             (HasLogger(logMsg))
import qualified Network.Wai.Handler.Warp as Warp
import qualified Servant
import           Servant                  (Proxy(..), type (:<|>)(..), ServerT, Context(EmptyContext), hoistServer,
                                           serveWithContext, Application, runHandler')
import           Server.Endpoints.Funds   (FundsApi, fundsHandler)
import           Server.Endpoints.Mint    (HasMintEndpoint, MintApi, mintHandler, processQueue)
import           Server.Endpoints.Ping    (PingApi, pingHandler)
import           Server.Internal          (AppM(unAppM), Env, loadEnv)
import           System.IO                (stdout, BufferMode(LineBuffering), hSetBuffering)

type ServerAPI s
    =    PingApi
    :<|> MintApi s
    :<|> FundsApi

type ServerConstraints s =
    ( HasMintEndpoint s
    , Servant.HasServer (ServerAPI s) '[]
    )

server :: HasMintEndpoint s => ServerT (ServerAPI s) (AppM s)
server = pingHandler
    :<|> mintHandler
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
        logStart env "Starting server..."
        Warp.run port $ mkApp @s env
    where
        logStart env = runExceptT . runHandler' . flip runReaderT env . unAppM . logMsg

mkApp :: forall s. ServerConstraints s => Env s -> Application
mkApp env = serveWithContext (serverAPI @s) EmptyContext $
    hoistServer (serverAPI @s) ((`runReaderT` env) . unAppM) server