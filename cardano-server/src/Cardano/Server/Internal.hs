{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Cardano.Server.Internal where

import           Cardano.Node.Emulator             (Params (..), pParamsFromProtocolParams)
import           Cardano.Server.Config             (Config (..), HyperTextProtocol (..), ServerEndpoint, decodeOrErrorFromFile, schemeFromProtocol)
import           Cardano.Server.Error              (Envelope)
import           Cardano.Server.Error.CommonErrors (InternalServerError (NoWalletProvided))
import           Cardano.Server.Input              (InputContext)
import           Cardano.Server.Utils.Logger       (HasLogger (..), Logger, logger)
import           Cardano.Server.Utils.Wait         (waitTime)
import           Cardano.Server.WalletEncryption   (loadWallet)
import           Control.Concurrent                (MVar, newEmptyMVar)
import           Control.Concurrent.Async          (async, wait)
import           Control.Exception                 (SomeException, throw)
import           Control.Lens                      ((^?))
import           Control.Monad.Catch               (MonadCatch, MonadThrow (..))
import           Control.Monad.Except              (MonadError (throwError))
import           Control.Monad.Extra               (join, whenM)
import           Control.Monad.IO.Class            (MonadIO (..))
import           Control.Monad.Reader              (MonadReader, ReaderT (ReaderT, runReaderT), asks, local)
import           Data.Aeson                        (fromJSON)
import qualified Data.Aeson                        as J
import           Data.Aeson.Lens                   (key)
import           Data.Functor                      ((<&>))
import           Data.IORef                        (IORef, newIORef)
import           Data.Kind                         (Type)
import           Data.Maybe                        (fromMaybe)
import           Data.Sequence                     (Seq, empty)
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import           GHC.Stack                         (HasCallStack)
import           Ledger                            (Address, NetworkId, TxOutRef)
import           Network.HTTP.Client               (defaultManagerSettings, newManager)
import           Network.HTTP.Client.TLS           (tlsManagerSettings)
import qualified PlutusAppsExtra.Api.Blockfrost    as BF
import           PlutusAppsExtra.IO.ChainIndex     (ChainIndex, HasChainIndex (..))
import           PlutusAppsExtra.IO.Wallet         (HasWallet (..), RestoredWallet)
import           PlutusAppsExtra.Types.Tx          (TransactionBuilder)
import           Servant                           (Handler, err404)
import qualified Servant
import qualified Servant.Client                    as Servant

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

instance HasWallet (ServerM api) where
    getRestoredWallet = asks envWallet <&> fromMaybe (throw NoWalletProvided)

instance HasChainIndex (ServerM api) where
    getChainIndex = asks envChainIndex

type family TxApiRequestOf api :: Type

type family InputOf api :: Type

type family AuxillaryEnvOf api :: Type

type InputWithContext api = (InputOf api, InputContext)

data QueueElem api = QueueElem
    { qeInput   :: InputOf api
    , qeContext :: InputContext
    , qeMvar    :: MVar (Either SomeException ())
    }

newQueueElem :: MonadIO m => InputWithContext api -> m (QueueElem api)
newQueueElem (qeInput, qeContext) = do
    qeMvar <- liftIO newEmptyMVar
    pure QueueElem{..}

type Queue api = Seq (QueueElem api)

type QueueRef api = IORef (Queue api)

class HasStatusEndpoint api where
    type StatusEndpointErrorsOf  api :: [Type]
    type StatusEndpointReqBodyOf api :: Type
    type StatusEndpointResOf     api :: Type
    statusHandler :: StatusHandler api

type StatusHandler api = StatusEndpointReqBodyOf api -> ServerM api (Envelope (StatusEndpointErrorsOf api) (StatusEndpointResOf api))

class HasVersionEndpoint api where
    type VersionEndpointResOf api :: Type
    versionHandler :: VersionHandler api

type VersionHandler api = ServerM api (VersionEndpointResOf api)

data ServerHandle api = ServerHandle
    { shDefaultCI              :: ChainIndex
    , shAuxiliaryEnv           :: AuxillaryEnvOf api
    , shGetTrackedAddresses    :: ServerM api [Address]
    , shTxEndpointsTxBuilders  :: InputOf api -> ServerM api [TransactionBuilder ()]
    , shServerIdle             :: ServerM api ()
    , shProcessRequest         :: TxApiRequestOf api -> ServerM api (InputWithContext api)
    , shStatusHandler          :: StatusHandler api
    , shCheckIfStatusAlive     :: ServerM api (Either Text ())
    , shVersionHandler         :: VersionHandler api
    }

data Env api = Env
    { envPort                  :: Int
    , envHost                  :: Text
    , envHyperTextProtocol     :: HyperTextProtocol
    , envQueueRef              :: QueueRef api
    , envWallet                :: Maybe RestoredWallet
    , envBfToken               :: Maybe BF.BfToken
    , envMinUtxosNumber        :: Int
    , envMaxUtxosNumber        :: Int
    , envLedgerParams          :: Params
    , envCollateral            :: Maybe TxOutRef
    , envNodeFilePath          :: FilePath
    , envChainIndex            :: ChainIndex
    , envActiveEndpoints       :: [ServerEndpoint]
    , envLogger                :: Logger (ServerM api)
    , envLoggerFilePath        :: Maybe FilePath
    , envServerHandle          :: ServerHandle api
    , envDiagnosticsInterval   :: Int
    }

serverTrackedAddresses :: ServerM api [Address]
serverTrackedAddresses = join $ asks $ shGetTrackedAddresses . envServerHandle

txEndpointsTxBuilders :: InputOf api -> ServerM api [TransactionBuilder ()]
txEndpointsTxBuilders input = asks (shTxEndpointsTxBuilders . envServerHandle) >>= ($ input)

serverIdle :: ServerM api ()
serverIdle = do
    delay <- liftIO $ async $ waitTime 2
    join $ asks $ shServerIdle . envServerHandle
    liftIO $ wait delay

txEndpointProcessRequest :: TxApiRequestOf api -> ServerM api (InputWithContext api)
txEndpointProcessRequest req = asks (shProcessRequest . envServerHandle) >>= ($ req)

getQueueRef :: ServerM api (QueueRef api)
getQueueRef = asks envQueueRef

getNetworkId :: ServerM api NetworkId
getNetworkId = asks $ pNetworkId . envLedgerParams

getAuxillaryEnv :: ServerM api (AuxillaryEnvOf api)
getAuxillaryEnv = asks $ shAuxiliaryEnv . envServerHandle

loadEnv :: HasCallStack
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
    let envPort                = cPort
        envHost                = cHost
        envHyperTextProtocol   = cHyperTextProtocol
        envMinUtxosNumber      = cMinUtxosNumber
        envMaxUtxosNumber      = cMaxUtxosNumber
        envDiagnosticsInterval = fromMaybe 300 cDiagnosticsInterval
        envLedgerParams        = Params slotConfig (pParamsFromProtocolParams pp) cNetworkId
        envActiveEndpoints     = cActiveEndpoints
        envCollateral          = cCollateral
        envNodeFilePath        = cNodeFilePath
        envChainIndex          = fromMaybe shDefaultCI cChainIndex
        envBfToken             = cBfToken
        envLogger              = logger
        envLoggerFilePath      = Nothing
        envServerHandle        = ServerHandle{..}
    pure Env{..}

setLoggerFilePath :: FilePath -> ServerM api a -> ServerM api a
setLoggerFilePath fp = local (\Env{..} -> Env{envLoggerFilePath = Just fp, ..})

checkEndpointAvailability :: ServerEndpoint -> ServerM api ()
checkEndpointAvailability endpoint = whenM (asks ((endpoint `notElem`) . envActiveEndpoints)) $ throwError err404

mkServerClientEnv :: ServerM api Servant.ClientEnv
mkServerClientEnv = do
        port     <- asks envPort
        host     <- asks envHost
        protocol <- asks envHyperTextProtocol
        manager <- liftIO $ newManager $ case protocol of
            HTTP  -> defaultManagerSettings
            HTTPS -> tlsManagerSettings
        pure $ Servant.ClientEnv
            manager
            (Servant.BaseUrl (schemeFromProtocol protocol) (T.unpack host) port "")
            Nothing
            Servant.defaultMakeClientRequest