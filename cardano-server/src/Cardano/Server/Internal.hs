{-# LANGUAGE OverloadedStrings #-}

module Cardano.Server.Internal where

import           Cardano.Server.Config          (Config (..), Creds, HasCreds, HyperTextProtocol (..), schemeFromProtocol)
import           Cardano.Server.EndpointName    (EndpointNames (EndpointNames), GetEdpointNames (getEndpointNames))
import           Cardano.Server.Error.Class     (InternalServerError (..))
import           Cardano.Server.Utils.Logger    (HasLogger (..), Logger, logger)
import           Control.Exception              (throw)
import           Control.Monad.Catch            (MonadCatch, MonadThrow (..))
import           Control.Monad.Except           (MonadError (..))
import           Control.Monad.Extra            (join, liftM3)
import           Control.Monad.IO.Class         (MonadIO (..))
import           Control.Monad.Morph            (MFunctor (hoist))
import           Control.Monad.Reader           (MonadReader, ReaderT (ReaderT, runReaderT), asks, local)
import           Data.Default                   (Default (def))
import           Data.Functor                   ((<&>))
import           Data.Kind                      (Type)
import           Data.Maybe                     (fromMaybe)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           GHC.Records                    (HasField (getField))
import           GHC.TypeLits                   (Symbol)
import           Ledger                         (NetworkId)
import           Network.Connection             (TLSSettings (TLSSettings))
import           Network.HTTP.Client            (defaultManagerSettings, newManager)
import           Network.HTTP.Client.TLS        (mkManagerSettings)
import           Network.TLS                    (ClientHooks (onCertificateRequest, onServerCertificate),
                                                 ClientParams (clientHooks, clientSupported), Supported (supportedCiphers),
                                                 credentialLoadX509FromMemory, defaultParamsClient)
import           Network.TLS.Extra.Cipher       (ciphersuite_default)
import           PlutusAppsExtra.Api.Blockfrost (BlockfrostToken, MonadBlockfrost (..))
import           PlutusAppsExtra.Api.Maestro    (MaestroToken, MonadMaestro (..))
import           PlutusAppsExtra.IO.ChainIndex  (ChainIndexProvider, HasChainIndexProvider (..))
import           PlutusAppsExtra.IO.Tx          (HasTxProvider (..), TxProvider)
import           PlutusAppsExtra.IO.Wallet      (HasWallet (..), HasWalletProvider (..), RestoredWallet, WalletProvider)
import           PlutusAppsExtra.Utils.Network  (HasNetworkId)
import qualified PlutusAppsExtra.Utils.Network  as Network
import           Servant                        (Handler)
import qualified Servant
import qualified Servant.Client                 as Servant
import           UnliftIO                       (MonadUnliftIO)

newtype AppT (api :: Type) (m :: Type -> Type) (a :: Type) = AppT {unAppT :: ReaderT (Env api) m a}
     deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MFunctor
        , MonadIO
        , MonadReader (Env api)
        , MonadThrow
        , MonadCatch
        )

deriving newtype instance MonadUnliftIO (AppT api IO)

runAppT :: Env api -> AppT api m a -> m a
runAppT env = (`runReaderT` env) . unAppT

type ServerM api = AppT api Handler
deriving newtype instance MonadError Servant.ServerError (ServerM api)

runServerM :: Env api -> ServerM api a -> IO a
runServerM env = fmap (either throw id) . Servant.runHandler . (`runReaderT` env) . unAppT

instance MonadIO m => HasLogger (AppT api m) where
    getLogger = do
        l <- asks envLogger
        pure $ \t -> hoist liftIO $ l t
    getLoggerFilePath = asks envLoggerFilePath

type family AuxillaryEnvOf api :: Type

type EnvWith (field :: Symbol) a api = HasField field (AuxillaryEnvOf api) a

getEnvField :: forall (field :: Symbol) a api m. Monad m => EnvWith field a api => AppT api m a
getEnvField = getField @field <$> getAuxillaryEnv

instance {-# OVERLAPS #-} (EnvWith "envNetworkId" NetworkId api, Monad m)
    => HasNetworkId (AppT api m) where
    getNetworkId = getField @"envNetworkId" <$> getAuxillaryEnv

instance {-# OVERLAPS #-} (EnvWith "envWallet" (Maybe RestoredWallet) api, HasNetworkId (AppT api m), MonadIO m, MonadThrow m)
    => HasWallet (AppT api m) where
    getRestoredWallet = getEnvField @"envWallet" <&> fromMaybe (throw NoWalletProvided)

instance {-# OVERLAPS #-} (EnvWith "envWalletProvider" WalletProvider api, HasWallet (AppT api m), HasNetworkId (AppT api m), MonadIO m
    , MonadThrow m)
    => HasWalletProvider (AppT api m) where
    getWalletProvider = getEnvField @"envWalletProvider"

instance {-# OVERLAPS #-} (EnvWith "envChainIndexProvider" ChainIndexProvider api, MonadMaestro (AppT api m), HasNetworkId (AppT api m)
    , MonadIO m, MonadCatch m)
    => HasChainIndexProvider (AppT api m) where
    getChainIndexProvider = getEnvField @"envChainIndexProvider"

instance {-# OVERLAPS #-} (EnvWith "envTxProvider" TxProvider api, HasChainIndexProvider (AppT api m), HasWalletProvider (AppT api m)
    , MonadMaestro (AppT api m), HasWallet (AppT api m), HasNetworkId (AppT api m), MonadIO m, MonadCatch m)
    => HasTxProvider (AppT api m) where
    getTxProvider = getEnvField @"envTxProvider"

instance {-# OVERLAPS #-} (EnvWith "envBlockfrostToken" (Maybe BlockfrostToken) api, HasNetworkId (AppT api m), MonadIO m, MonadCatch m)
    => MonadBlockfrost (AppT api m) where
    getBlockfrostToken = getEnvField @"envBlockfrostToken" >>= maybe (throwM NoBlockfrostToken) pure

instance {-# OVERLAPS #-} (EnvWith "envMaestroToken" (Maybe MaestroToken) api, HasNetworkId (AppT api m), MonadIO m, MonadCatch m)
    => MonadMaestro (AppT api m) where
    getMaestroToken = getEnvField @"envMaestroToken" >>= maybe (throwM NoMaestroToken) pure

data Env api = Env
    { envPort              :: Int
    , envHost              :: Text
    , envHyperTextProtocol :: HyperTextProtocol
    , envCreds             :: Creds
    , envActiveEndpoints   :: EndpointNames api
    , envLogger            :: Logger (AppT api IO)
    , envLoggerFilePath    :: Maybe FilePath
    , envAuxilaryEnv       :: AuxillaryEnvOf api
    }

getAuxillaryEnv :: Monad m => AppT api m (AuxillaryEnvOf api)
getAuxillaryEnv = asks envAuxilaryEnv

loadEnv :: forall api. (HasCreds, GetEdpointNames api)
  => Config api
  -> AuxillaryEnvOf api
  -> IO (Env api)
loadEnv Config{..} envAuxilaryEnv = do
    let envPort              = cPort
        envHost              = cHost
        envHyperTextProtocol = cHyperTextProtocol
        envCreds             = ?creds
        envActiveEndpoints   = fromMaybe (EndpointNames $ getEndpointNames @api) cActiveEndpoints
        envLogger            = logger
        envLoggerFilePath    = Nothing
    pure Env{..}

setLoggerFilePath :: Monad m => FilePath -> AppT api m a -> AppT api m a
setLoggerFilePath fp = local (\Env{..} -> Env{envLoggerFilePath = Just fp, ..})

mkServantClientEnv :: (MonadIO m, HasCreds) => Int -> Text -> HyperTextProtocol -> m Servant.ClientEnv
mkServantClientEnv port host protocol = do
    manager <- liftIO $ newManager $ case (protocol, ?creds) of
        (HTTP, _)                     -> defaultManagerSettings
        (HTTPS, Nothing)              -> mkManagerSettings def def
        (HTTPS, Just (certBs, keyBs)) -> mkManagerSettingsWithCreds certBs keyBs
    pure $ Servant.ClientEnv
        manager
        (Servant.BaseUrl (schemeFromProtocol protocol) (T.unpack host) port "")
        Nothing
        Servant.defaultMakeClientRequest
    where
        mkManagerSettingsWithCreds certBs keyBs = do
            let creds = either error Just $ credentialLoadX509FromMemory certBs keyBs
                hooks = def
                    { onCertificateRequest = \_ -> return creds
                    , onServerCertificate  = \_ _ _ _ -> return []
                    }
                clientParams = (defaultParamsClient (T.unpack host)  "")
                    { clientHooks     = hooks
                    , clientSupported = def { supportedCiphers = ciphersuite_default }
                    }
                tlsSettings = TLSSettings clientParams
            mkManagerSettings tlsSettings Nothing

mkServantClientEnvFromConfig :: (MonadIO m, HasCreds) => Config api -> m Servant.ClientEnv
mkServantClientEnvFromConfig Config{..} = mkServantClientEnv cPort cHost cHyperTextProtocol

mkServerClientEnv :: MonadIO m => AppT api m Servant.ClientEnv
mkServerClientEnv = do
    creds <- asks envCreds
    let ?creds = creds
    join $ liftM3 mkServantClientEnv (asks envPort) (asks envHost) (asks envHyperTextProtocol)