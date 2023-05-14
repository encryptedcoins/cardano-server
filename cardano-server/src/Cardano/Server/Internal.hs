{-# LANGUAGE AllowAmbiguousTypes          #-}
{-# LANGUAGE ConstraintKinds              #-}
{-# LANGUAGE DataKinds                    #-}
{-# LANGUAGE DerivingVia                  #-}
{-# LANGUAGE ExistentialQuantification    #-}
{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE FlexibleInstances            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving   #-}
{-# LANGUAGE MultiParamTypeClasses        #-}
{-# LANGUAGE OverloadedStrings            #-}
{-# LANGUAGE RankNTypes                   #-}
{-# LANGUAGE RecordWildCards              #-}
{-# LANGUAGE ScopedTypeVariables          #-}
{-# LANGUAGE TypeFamilies                 #-}
{-# LANGUAGE UndecidableInstances         #-}

module Cardano.Server.Internal where

import           Cardano.Node.Emulator             (Params (..), pParamsFromProtocolParams)
import           Cardano.Server.Config             (Config (..), ServerEndpoint, decodeOrErrorFromFile, loadConfig)
import           Cardano.Server.Error              (Envelope)
import           Cardano.Server.Error.CommonErrors (InternalServerError (NoWalletProvided))
import           Cardano.Server.Input              (InputContext)
import           Cardano.Server.Utils.Logger       (HasLogger (..), Logger, logger)
import           Cardano.Server.WalletEncryption   (loadWallet)
import           Control.Exception                 (throw)
import           Control.Monad.Catch               (MonadCatch, MonadThrow (..))
import           Control.Monad.Except              (MonadError (throwError))
import           Control.Monad.Extra               (join, whenM)
import           Control.Monad.IO.Class            (MonadIO)
import           Control.Monad.Reader              (MonadReader, ReaderT (ReaderT, runReaderT), asks, local)
import           Data.Default                      (Default (..))
import           Data.Functor                      ((<&>))
import           Data.IORef                        (IORef, newIORef)
import           Data.Kind                         (Type)
import           Data.Maybe                        (fromMaybe)
import           Data.Sequence                     (Seq, empty)
import           GHC.Stack                         (HasCallStack)
import           Ledger                            (Address, NetworkId, TxOutRef)
import qualified PlutusAppsExtra.IO.Blockfrost     as BF
import           PlutusAppsExtra.IO.ChainIndex     (ChainIndex, HasChainIndex (..))
import           PlutusAppsExtra.IO.Wallet         (HasWallet (..), RestoredWallet)
import           PlutusAppsExtra.Types.Tx          (TransactionBuilder)
import           Servant                           (Handler, err404)
import qualified Servant

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

type Queue api = Seq (InputWithContext api)

type QueueRef api = IORef (Queue api)

class HasStatusEndpoint api where
    type StatusEndpointErrorsOf  api :: [Type]
    type StatusEndpointReqBodyOf api :: Type
    type StatusEndpointResOf     api :: Type
    statusHandler :: StatusHandler api

type StatusHandler api = StatusEndpointReqBodyOf api -> ServerM api (Envelope (StatusEndpointErrorsOf api) (StatusEndpointResOf api))

data ServerHandle api = ServerHandle
    { shDefaultCI              :: ChainIndex
    , shAuxiliaryEnv           :: AuxillaryEnvOf api
    , shGetTrackedAddresses    :: ServerM api [Address]
    , shTxEndpointsTxBuilders  :: InputOf api -> ServerM api [TransactionBuilder ()]
    , shServerIdle             :: ServerM api ()
    , shProcessRequest         :: TxApiRequestOf api -> ServerM api (InputWithContext api)
    , shStatusHandler          :: StatusHandler api
    }

data Env api = Env
    { envPort                  :: Int
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
    }

serverTrackedAddresses :: ServerM api [Address]
serverTrackedAddresses = join $ asks $ shGetTrackedAddresses . envServerHandle

txEndpointsTxBuilders :: InputOf api -> ServerM api [TransactionBuilder ()]
txEndpointsTxBuilders input = asks (shTxEndpointsTxBuilders . envServerHandle) >>= ($ input)

serverIdle :: ServerM api ()
serverIdle = join $ asks $ shServerIdle . envServerHandle

txEndpointProcessRequest :: TxApiRequestOf api -> ServerM api (InputWithContext api)
txEndpointProcessRequest req = asks (shProcessRequest . envServerHandle) >>= ($ req)

getQueueRef :: ServerM api (QueueRef api)
getQueueRef = asks envQueueRef

getNetworkId :: ServerM api NetworkId
getNetworkId = asks $ pNetworkId . envLedgerParams

getAuxillaryEnv :: ServerM api (AuxillaryEnvOf api)
getAuxillaryEnv = asks $ shAuxiliaryEnv . envServerHandle

loadEnv :: HasCallStack => ServerHandle api -> IO (Env api)
loadEnv ServerHandle{..} = do
        Config{..}   <- loadConfig
        envQueueRef  <- newIORef empty
        envWallet    <- sequence $ loadWallet <$> cWalletFile
        pp <- decodeOrErrorFromFile cProtocolParametersFile
        let envPort              = cPort
            envMinUtxosNumber    = cMinUtxosNumber
            envMaxUtxosNumber    = cMaxUtxosNumber
            envLedgerParams      = Params def (pParamsFromProtocolParams pp) cNetworkId
            envActiveEndpoints   = cActiveEndpoints
            envCollateral        = cCollateral
            envNodeFilePath      = cNodeFilePath
            envChainIndex        = fromMaybe shDefaultCI cChainIndex
            envBfToken           = cBfToken
            envLogger            = logger
            envLoggerFilePath    = Nothing
            envServerHandle      = ServerHandle{..}
        pure Env{..}

setLoggerFilePath :: FilePath -> ServerM api a -> ServerM api a
setLoggerFilePath fp = local (\Env{..} -> Env{envLoggerFilePath = Just fp, ..})

checkEndpointAvailability :: ServerEndpoint -> ServerM api ()
checkEndpointAvailability endpoint = whenM (asks ((endpoint `notElem`) . envActiveEndpoints)) $ throwError err404