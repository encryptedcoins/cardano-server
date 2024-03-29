{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

module Cardano.Server.Diagnostics where

import           Cardano.Server.Endpoints.Ping         (PingApi)
import           Cardano.Server.Internal               (ServerM, mkServerClientEnv)
import           Cardano.Server.Utils.Logger           (HasLogger, logMsg, (.<))
import           Cardano.Server.Utils.Wait             (waitTime)
import qualified Cardano.Wallet.Api.Types              as Wallet
import qualified Cardano.Wallet.Primitive.SyncProgress as Wallet (SyncProgress (NotResponding))
import           Control.Concurrent.Async              (async, wait)
import           Control.Monad                         (forever, when)
import           Control.Monad.Catch                   (Exception, MonadCatch, MonadThrow (throwM), SomeException, handle)
import           Control.Monad.Extra                   (whenM)
import           Control.Monad.IO.Class                (MonadIO (..))
import           Data.Data                             (Proxy (..))
import           Data.Foldable.Extra                   (orM)
import           Data.Text                             (Text)
import           PlutusAppsExtra                       (nodeHealthCheck)
import qualified PlutusAppsExtra.Api.Kupo              as Kupo
import           PlutusAppsExtra.IO.ChainIndex         (ChainIndexProvider (Plutus), HasChainIndexProvider (getChainIndexProvider))
import qualified PlutusAppsExtra.IO.ChainIndex         as ChainIndex
import           PlutusAppsExtra.IO.Tx                 (HasTxProvider (getTxProvider))
import qualified PlutusAppsExtra.IO.Tx                 as Tx
import           PlutusAppsExtra.IO.Wallet             (HasWalletProvider (getWalletProvider))
import qualified PlutusAppsExtra.IO.Wallet             as Wallet
import qualified PlutusAppsExtra.IO.Wallet.Cardano     as Wallet
import qualified PlutusAppsExtra.Utils.Kupo            as Kupo
import           Servant.Client                        (ClientM)
import qualified Servant.Client                        as Servant

doDiagnostics :: MonadIO m => Int -> m () -> m ()
doDiagnostics i d = forever $ do
    delay <- liftIO $ async $ waitTime i
    d
    liftIO $ wait delay

providersDiagnostics :: (MonadIO m, MonadCatch m, HasLogger m, HasTxProvider m) => m ()
providersDiagnostics = do
    whenM (orM [(== Wallet.Cardano) <$> getWalletProvider, (== Tx.Cardano) <$> getTxProvider, (== Plutus) <$> getChainIndexProvider])
        $ withDiagnostics "cardano-node" nodeDiagnostics
    whenM ((== Wallet.Cardano) <$> getWalletProvider)
        $ withDiagnostics "cardano-wallet" cardanoWalletDiagnostics
    whenM ((== ChainIndex.Kupo) <$> getChainIndexProvider)
        $ withDiagnostics "kupo" kupoDiagnostics

pingDiagnostics :: ServerM api ()
pingDiagnostics = clientDiagnostics (Servant.client (Proxy @PingApi))

clientDiagnostics :: ClientM a -> ServerM api ()
clientDiagnostics client = withDiagnostics "server" $ do
    env <- mkServerClientEnv
    res <- liftIO $ Servant.runClientM client env
    either throwM (const $ logMsg "Server is alive.") res

cardanoWalletDiagnostics :: (MonadIO m, MonadCatch m, HasLogger m, HasWalletProvider m) => m ()
cardanoWalletDiagnostics = whenM ((== Wallet.Cardano) <$> getWalletProvider) $ withDiagnostics "cardano-wallet" $ do
    res <- Wallet.getHealth
    when ((== Wallet.NotResponding) $ Wallet.getApiT $ Wallet.syncProgress res)
        $ throwM $ DiagnosticsError "Cardano-wallet sync progress: not responding."
    logMsg "Cardano-wallet is alive."

kupoDiagnostics :: (MonadIO m, MonadCatch m, HasLogger m, HasChainIndexProvider m) => m ()
kupoDiagnostics = whenM ((== ChainIndex.Kupo) <$> getChainIndexProvider) $ withDiagnostics "kupo" $ do
    response <- liftIO Kupo.getHealth
    if Kupo.ghrConnected response
    then logMsg "Kupo is alive."
    else logMsg $ "Kupo isn't alive. Received response: " .< response

nodeDiagnostics :: (MonadIO m, MonadCatch m, HasLogger m, HasTxProvider m) => m ()
nodeDiagnostics = whenM (orM
    [ (== Wallet.Cardano) <$> getWalletProvider
    , (== Tx.Cardano) <$> getTxProvider
    , (== ChainIndex.Plutus) <$> getChainIndexProvider
    ]) nodeDiagnosticsUnwrapped

nodeDiagnosticsUnwrapped :: (MonadIO m, MonadCatch m, HasLogger m) => m ()
nodeDiagnosticsUnwrapped = withDiagnostics "cardano-node" $ do
    liftIO nodeHealthCheck
    logMsg "Cardano-node is alive."

withDiagnostics :: (MonadCatch m, HasLogger m) => Text -> m () -> m ()
withDiagnostics serviceName diagnostics = handle h $ do
        logMsg $ "Checking if " <> serviceName <> " is alive..."
        diagnostics
    where
        h (e :: SomeException) = logMsg $ serviceName <> " isn't alive. Reason: " .< e

newtype DiagnosticsError = DiagnosticsError {getDiagnosticsError :: Text}
    deriving newtype (Show)
    deriving anyclass (Exception)