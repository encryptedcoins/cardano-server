{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

module Cardano.Server.Diagnostics where

import           Cardano.Server.Config                 (ServerEndpoint (PingE, StatusE))
import           Cardano.Server.Endpoints.Ping         (PingApi)
import           Cardano.Server.Internal               (Env (..), ServerM, mkServerClientEnv, shCheckIfStatusAlive)
import           Cardano.Server.Utils.Logger           (logMsg, (.<))
import           Cardano.Server.Utils.Wait             (waitTime)
import qualified Cardano.Wallet.Api.Types              as Wallet
import qualified Cardano.Wallet.Primitive.SyncProgress as Wallet (SyncProgress (NotResponding))
import           Control.Concurrent.Async              (async, wait)
import           Control.Monad                         (forever, join, when)
import           Control.Monad.Catch                   (Exception, MonadThrow (throwM), SomeException, handle)
import           Control.Monad.IO.Class                (MonadIO (..))
import           Control.Monad.Reader                  (asks)
import           Data.Data                             (Proxy (..))
import           Data.Text                             (Text)
import qualified PlutusAppsExtra.Api.Kupo              as Kupo
import qualified PlutusAppsExtra.IO.Node               as Node
import qualified PlutusAppsExtra.IO.Wallet             as Wallet
import qualified PlutusAppsExtra.Utils.Kupo            as Kupo
import qualified Servant.Client                        as Servant

doDiagnostics, nodeDiagnostics, walletDiagnostics, kupoDiagnostics, serverDiagnostics :: ServerM api ()
doDiagnostics = do
    i <- asks envDiagnosticsInterval
    forever $ do
        delay <- liftIO $ async $ waitTime i
        withDiagnostics "cardano-node" nodeDiagnostics
        withDiagnostics "cardano-wallet" walletDiagnostics
        withDiagnostics "kupo" kupoDiagnostics
        withDiagnostics "server" serverDiagnostics
        liftIO $ wait delay

serverDiagnostics = do
    enpoints <- asks envActiveEndpoints
    if  | StatusE `elem` enpoints -> checkStatus
        | PingE   `elem` enpoints -> checkPing
        | otherwise -> logMsg
            "Failed to check whether the server is alive: \
            \both ping and status endpoints was set to inactive in the configuration file."
    where
        checkStatus = do
            res <- join (asks (shCheckIfStatusAlive . envServerHandle))
            either (throwM . DiagnosticsError) (const $ logMsg "Server is alive.") res
        checkPing = do
            env <- mkServerClientEnv
            res <- liftIO $ Servant.runClientM (Servant.client (Proxy @PingApi)) env
            either throwM (const $ logMsg "Server is alive.") res

nodeDiagnostics = do
    liftIO Node.healthCheck
    logMsg "Cardano-node is alive."

walletDiagnostics = do
    res <- Wallet.getHealth
    when ((== Wallet.NotResponding) $ Wallet.getApiT $ Wallet.syncProgress res)
        $ throwM $ DiagnosticsError "Cardano-wallet sync progress: not responding."
    logMsg "Cardano-wallet is alive."

kupoDiagnostics = do
    response <- liftIO Kupo.getHealth
    if Kupo.ghrConnected response
    then logMsg "Kupo is alive."
    else logMsg $ "Kupo isn't alive. Received response: " .< response

withDiagnostics :: Text -> ServerM api () -> ServerM api ()
withDiagnostics serviceName diagnostics = handle h $ do
        logMsg $ "Checking if " <> serviceName <> " is alive..."
        diagnostics
    where
        h :: SomeException -> ServerM api ()
        h e = logMsg $ serviceName <> " isn't alive. Reason: " .< e

newtype DiagnosticsError = DiagnosticsError {getDiagnosticsError :: Text}
    deriving newtype  Show
    deriving anyclass Exception