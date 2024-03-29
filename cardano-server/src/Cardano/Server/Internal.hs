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
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Cardano.Server.Internal where

import           Cardano.Node.Emulator           (Params (..), pParamsFromProtocolParams)
import           Cardano.Server.Config           (CardanoServerConfig (..), Config (..), Creds, HasCreds, HyperTextProtocol (..),
                                                  decodeOrErrorFromFile, schemeFromProtocol)
import           Cardano.Server.Error            (InternalServerError (..))
import           Cardano.Server.Input            (InputContext)
import           Cardano.Server.Utils.Logger     (HasLogger (..), Logger, logger)
import           Cardano.Server.WalletEncryption (loadWallet)
import           Control.Concurrent              (MVar, newEmptyMVar)
import           Control.Exception               (SomeException, throw)
import           Control.Lens                    ((^?))
import           Control.Monad.Catch             (MonadCatch, MonadThrow (..))
import           Control.Monad.Except            (MonadError (throwError))
import           Control.Monad.Extra             (join, liftM3, whenM)
import           Control.Monad.IO.Class          (MonadIO (..))
import           Control.Monad.Reader            (MonadReader, ReaderT (ReaderT, runReaderT), asks, local)
import           Data.Aeson                      (fromJSON)
import qualified Data.Aeson                      as J
import           Data.Aeson.Lens                 (key)
import           Data.Default                    (Default (def))
import           Data.Functor                    ((<&>))
import           Data.IORef                      (IORef, newIORef)
import           Data.Kind                       (Type)
import           Data.Maybe                      (fromMaybe)
import           Data.Sequence                   (Seq, empty)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           GHC.Stack                       (HasCallStack)
import           Ledger                          (NetworkId, TxOutRef)
import           Network.Connection              (TLSSettings (TLSSettings))
import           Network.HTTP.Client             (ManagerSettings (managerResponseTimeout), defaultManagerSettings, newManager,
                                                  responseTimeoutMicro)
import           Network.HTTP.Client.TLS         (mkManagerSettings)
import           Network.TLS                     (ClientHooks (onCertificateRequest, onServerCertificate),
                                                  ClientParams (clientHooks, clientSupported), Supported (supportedCiphers),
                                                  credentialLoadX509FromMemory, defaultParamsClient)
import           Network.TLS.Extra.Cipher        (ciphersuite_default)
import           PlutusAppsExtra.Api.Blockfrost  (BlockfrostToken)
import           PlutusAppsExtra.Api.Maestro     (MaestroToken, MonadMaestro (..))
import           PlutusAppsExtra.IO.ChainIndex   (ChainIndexProvider, HasChainIndexProvider (..))
import           PlutusAppsExtra.IO.Tx           (HasTxProvider (..), TxProvider)
import qualified PlutusAppsExtra.IO.Tx           as Tx
import           PlutusAppsExtra.IO.Wallet       (HasWallet (..), HasWalletProvider (..), RestoredWallet, WalletProvider)
import qualified PlutusAppsExtra.IO.Wallet       as Wallet
import           PlutusAppsExtra.Utils.Network   (HasNetworkId)
import qualified PlutusAppsExtra.Utils.Network   as Network
import           Servant                         (Handler, err404)
import qualified Servant
import qualified Servant.Client                  as Servant

newtype ServerM api a = ServerM {unServerM :: ReaderT (Env api) Handler a}
     deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader (Env api)
        , MonadError Servant.ServerError
        , MonadThrow
        , MonadCatch
        )

runServerM :: Env api -> ServerM api a -> IO a
runServerM env = fmap (either throw id) . Servant.runHandler . (`runReaderT` env) . unServerM

instance HasLogger (ServerM api) where
    getLogger = asks envLogger
    getLoggerFilePath = asks envLoggerFilePath

instance HasNetworkId (ServerM api) where
    getNetworkId = getNetworkId

instance HasWallet (ServerM api) where
    getRestoredWallet = asks envWallet <&> fromMaybe (throw NoWalletProvided)

instance HasChainIndexProvider (ServerM api) where
    getChainIndexProvider = asks envChainIndexProvider

instance MonadMaestro (ServerM api) where
    getMaestroToken = asks envMaestroToken >>= maybe (throwM NoMaestroToken) pure

instance HasWalletProvider (ServerM api) where
    getWalletProvider = asks envWalletProvider

instance HasTxProvider (ServerM api) where
    getTxProvider = asks envTxProvider

type family AuxillaryEnvOf api :: Type

data ServerHandle api = ServerHandle
    { shDefaultCI              :: ChainIndexProvider
    , shAuxiliaryEnv           :: AuxillaryEnvOf api
    }

data Env api = Env
    { envPort                  :: Int
    , envHost                  :: Text
    , envHyperTextProtocol     :: HyperTextProtocol
    , envCreds                 :: Creds
    , envWallet                :: Maybe RestoredWallet
    , envBlockfrostToken       :: Maybe BlockfrostToken
    , envMaestroToken          :: Maybe MaestroToken
    , envMinUtxosNumber        :: Int
    , envMaxUtxosNumber        :: Int
    , envLedgerParams          :: Params
    , envCollateral            :: Maybe TxOutRef
    , envNodeFilePath          :: FilePath
    , envWalletProvider        :: WalletProvider
    , envChainIndexProvider    :: ChainIndexProvider
    , envTxProvider            :: TxProvider
    , envActiveEndpoints       :: [Text]
    , envLogger                :: Logger (ServerM api)
    , envLoggerFilePath        :: Maybe FilePath
    , envServerHandle          :: ServerHandle api
    , envDiagnosticsInterval   :: Int
    }

instance CardanoServerConfig (Env api) where
    configHost = envHost
    configPort = envPort
    configHyperTextProtocol = envHyperTextProtocol

getNetworkId :: ServerM api NetworkId
getNetworkId = asks $ pNetworkId . envLedgerParams

getAuxillaryEnv :: ServerM api (AuxillaryEnvOf api)
getAuxillaryEnv = asks $ shAuxiliaryEnv . envServerHandle

loadEnv :: (HasCallStack, HasCreds)
  => Config
  -> ServerHandle api
  -> IO (Env api)
loadEnv Config{..} ServerHandle{..} = do
    envQueueRef  <- newIORef empty
    envWallet    <- sequence $ loadWallet <$> cWalletFile
    pp <- decodeOrErrorFromFile cProtocolParametersFile
    slotConfig <- do
        val <- decodeOrErrorFromFile @J.Value cSlotConfigFile
        case val ^? key "cicSlotConfig" <&> fromJSON of
            Just (J.Success sc) -> pure sc
            _                   -> error "There is no slot config in chain index config file."
    envBlockfrostToken <- sequence $ decodeOrErrorFromFile <$> cBfTokenFilePath
    envMaestroToken    <- sequence $ decodeOrErrorFromFile <$> cMaestroTokenFilePath
    let envPort                = cPort
        envHost                = cHost
        envHyperTextProtocol   = cHyperTextProtocol
        envCreds               = ?creds
        envMinUtxosNumber      = cMinUtxosNumber
        envMaxUtxosNumber      = cMaxUtxosNumber
        envDiagnosticsInterval = fromMaybe 300 cDiagnosticsInterval
        envLedgerParams        = Params slotConfig (pParamsFromProtocolParams pp) cNetworkId
        envActiveEndpoints     = cActiveEndpoints
        envCollateral          = cCollateral
        envNodeFilePath        = cNodeFilePath
        envWalletProvider      = fromMaybe Wallet.Cardano cWalletProvider
        envChainIndexProvider  = fromMaybe shDefaultCI cChainIndexProvider
        envTxProvider          = fromMaybe Tx.Cardano cTxProvider
        envLogger              = logger
        envLoggerFilePath      = Nothing
        envServerHandle        = ServerHandle{..}
    pure Env{..}

setLoggerFilePath :: FilePath -> ServerM api a -> ServerM api a
setLoggerFilePath fp = local (\Env{..} -> Env{envLoggerFilePath = Just fp, ..})

checkEndpointAvailability :: Text -> ServerM api ()
checkEndpointAvailability endpoint = whenM (asks ((endpoint `notElem`) . envActiveEndpoints)) $ throwError err404

mkServantClientEnv :: (MonadIO m, HasCreds) => Int -> Text -> HyperTextProtocol -> m Servant.ClientEnv
mkServantClientEnv port host protocol = do
    manager <- liftIO $ newManager $ case (protocol, ?creds) of
        (HTTP, _)                     -> setTimeout   defaultManagerSettings
        (HTTPS, Nothing)              -> setTimeout $ mkManagerSettings def def
        (HTTPS, Just (certBs, keyBs)) -> setTimeout $ mkManagerSettingsWithCreds certBs keyBs
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
        setTimeout settings = settings{managerResponseTimeout = responseTimeoutMicro 90_000_000}

mkServerClientEnv :: ServerM api Servant.ClientEnv
mkServerClientEnv = do
    creds <- asks envCreds
    let ?creds = creds
    join $ liftM3 mkServantClientEnv (asks envPort) (asks envHost) (asks envHyperTextProtocol)