{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Server.Internal where

import           Cardano.Server.Config         (Config (..), Creds, HasCreds, HyperTextProtocol (..), schemeFromProtocol)
import           Cardano.Server.Utils.Logger   (HasLogger (..), Logger, logger)
import           Cardano.Server.Utils.Wait     (waitTime)
import           Control.Concurrent.Async      (async, wait)
import           Control.Exception             (throw)
import           Control.Monad                 (when)
import           Control.Monad.Catch           (MonadCatch, MonadThrow (..))
import           Control.Monad.Except          (MonadError (throwError))
import           Control.Monad.Extra           (join, liftM3)
import           Control.Monad.IO.Class        (MonadIO (..))
import           Control.Monad.Morph           (MFunctor (..))
import           Control.Monad.Reader          (MonadReader, ReaderT (ReaderT, runReaderT), asks, local)
import           Data.Default                  (Default (def))
import           Data.Kind                     (Type)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           GHC.Records                   (HasField (..))
import           GHC.Stack                     (HasCallStack)
import           GHC.TypeLits                  (Symbol)
import           Ledger                        (NetworkId)
import           Network.Connection            (TLSSettings (TLSSettings))
import           Network.HTTP.Client           (defaultManagerSettings, newManager)
import           Network.HTTP.Client.TLS       (mkManagerSettings)
import           Network.TLS                   (ClientHooks (onCertificateRequest, onServerCertificate),
                                                ClientParams (clientHooks, clientSupported), Supported (supportedCiphers),
                                                credentialLoadX509FromMemory, defaultParamsClient)
import           Network.TLS.Extra.Cipher      (ciphersuite_default)
import           PlutusAppsExtra.Utils.Network (HasNetworkId (..))
import           Servant                       (Handler, err404)
import qualified Servant
import qualified Servant.Client                as Servant
import           UnliftIO                      (MonadUnliftIO)

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

deriving instance MonadUnliftIO (AppT api IO)

runAppT :: Env api -> AppT api m a -> m a
runAppT env = (`runReaderT` env) . unAppT

type ServerM api = AppT api Handler
deriving instance MonadError Servant.ServerError (ServerM api)

runServerM :: Env api -> ServerM api a -> IO a
runServerM env = fmap (either throw id) . Servant.runHandler . (`runReaderT` env) . unAppT

instance MonadIO m => HasLogger (AppT api m) where
    getLogger = do
        l <- asks envLogger
        pure $ \t -> hoist liftIO $ l t
    getLoggerFilePath = asks envLoggerFilePath

type family AuxillaryEnvOf api :: Type

data Env api = Env
    { envPort                :: Int
    , envHost                :: Text
    , envHyperTextProtocol   :: HyperTextProtocol
    , envCreds               :: Creds
    , envActiveEndpoints     :: Maybe [Text]
    , envLogger              :: Logger (AppT api IO)
    , envLoggerFilePath      :: Maybe FilePath
    , envServerIdle          :: ServerM api ()
    , envAuxilaryEnv         :: AuxillaryEnvOf api
    }

getAuxillaryEnv :: Monad m => AppT api m (AuxillaryEnvOf api)
getAuxillaryEnv = asks envAuxilaryEnv

serverIdle :: ServerM api ()
serverIdle = do
    delay <- liftIO $ async $ waitTime 2
    join $ asks envServerIdle
    liftIO $ wait delay

type EnvWith (field :: Symbol) a api = HasField field (AuxillaryEnvOf api) a

getEnvField :: forall (field :: Symbol) a api m. Monad m => EnvWith field a api => AppT api m a
getEnvField = getField @field <$> getAuxillaryEnv

instance {-# OVERLAPS #-} (EnvWith "envNetworkId" NetworkId api, Monad m)
    => HasNetworkId (AppT api m) where
    getNetworkId = getField @"envNetworkId" <$> getAuxillaryEnv

loadEnv :: (HasCallStack, HasCreds)
  => Config api
  -> AuxillaryEnvOf api
  -> ServerM api ()
  -> IO (Env api)
loadEnv Config{..} envAuxilaryEnv envServerIdle = do
    let envPort              = cPort
        envHost              = cHost
        envHyperTextProtocol = cHyperTextProtocol
        envCreds             = ?creds
        envActiveEndpoints   = cActiveEndpoints
        envLogger            = logger
        envLoggerFilePath    = Nothing
    pure Env{..}

setLoggerFilePath :: FilePath -> ServerM api a -> ServerM api a
setLoggerFilePath fp = local (\Env{..} -> Env{envLoggerFilePath = Just fp, ..})

checkEndpointAvailability :: Text -> ServerM api ()
checkEndpointAvailability endpoint = do
        mbEndpoints <- asks envActiveEndpoints
        maybe (pure ()) (\es -> when (check es) $ throwError err404) mbEndpoints
    where
        check es = endpoint `elem` es

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

mkServerClientEnv :: ServerM api Servant.ClientEnv
mkServerClientEnv = do
    creds <- asks envCreds
    let ?creds = creds
    join $ liftM3 mkServantClientEnv (asks envPort) (asks envHost) (asks envHyperTextProtocol)