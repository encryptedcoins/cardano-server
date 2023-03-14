{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Cardano.Server.Internal where

import           Cardano.Node.Emulator             (Params (..), pParamsFromProtocolParams)
import           Cardano.Server.Config             (Config (..), InactiveEndpoints, decodeOrErrorFromFile, loadConfig)
import           Cardano.Server.Error.CommonErrors (InternalServerError (NoWalletProvided))
import           Cardano.Server.Input              (InputContext)
import           Cardano.Server.Utils.Logger       (HasLogger (..))
import           Control.Exception                 (throw)
import           Control.Monad.Catch               (MonadCatch, MonadThrow (..))
import           Control.Monad.Except              (MonadError)
import           Control.Monad.Extra               (join, whenM)
import           Control.Monad.IO.Class            (MonadIO)
import           Control.Monad.Reader              (MonadReader, ReaderT (ReaderT, runReaderT), asks)
import           Data.Default                      (Default (..))
import           Data.Functor                      ((<&>))
import           Data.IORef                        (IORef, newIORef)
import           Data.Kind                         (Type)
import           Data.Maybe                        (fromMaybe)
import           Data.Sequence                     (Seq, empty)
import           Ledger                            (Address, NetworkId, TxOutRef)
import qualified PlutusAppsExtra.IO.Blockfrost     as BF
import           PlutusAppsExtra.IO.ChainIndex     (ChainIndex, HasChainIndex (..))
import           PlutusAppsExtra.IO.Wallet         (HasWallet (..), RestoredWallet, getWalletAddr)
import           PlutusAppsExtra.Types.Tx          (TransactionBuilder)
import           Servant                           (Handler, err404)
import qualified Servant
-- class HasAssociatedApi (m :: Type -> Type) where
--     type AssociatedApi m :: Type

type CardanoServerMonad (m :: Type -> Type) =
    ( Monad m
    , MonadIO m
    , MonadReader (Env (AssociatedApi m)) m
    , MonadThrow m
    , MonadCatch m
    , HasWallet m
    , HasChainIndex m
    , HasLogger m

    , Show (InputOf (AssociatedApi m))
    , Show (TxApiRequestOf (AssociatedApi m))
    )

type family TxApiRequestOf api :: Type

type family InputOf api :: Type

type InputWithContext api = (InputOf api, InputContext)

type Queue api = Seq (InputWithContext api)

type QueueRef api = IORef (Queue api)

type EnvTrackedAddresses   = forall (m :: Type -> Type).  CardanoServerMonad m => m [Address]
type EnvTxBuilders api     = forall (m :: Type -> Type). (CardanoServerMonad m, (api ~ (AssociatedApi m))) => InputOf api -> m [TransactionBuilder ()]
type EnvServerIdle         = forall (m :: Type -> Type).  CardanoServerMonad m => m ()
type EnvProcessRequest api = forall (m :: Type -> Type). (CardanoServerMonad m, api ~ AssociatedApi m) => TxApiRequestOf api -> m (InputWithContext api)

data Env api = Env
    { envQueueRef              :: QueueRef api
    , envWallet                :: Maybe RestoredWallet
    -- , envAuxiliary          :: AuxiliaryEnvOf api
    , envBfToken               :: BF.BfToken
    , envMinUtxosNumber        :: Int
    , envMaxUtxosNumber        :: Int
    , envLedgerParams          :: Params
    , envCollateral            :: Maybe TxOutRef
    , envNodeFilePath          :: FilePath
    , envChainIndex            :: ChainIndex
    , envInactiveEndpoints     :: InactiveEndpoints
    , envTrackedAddresses      :: EnvTrackedAddresses
    , envTxEndpointsTxBuilders :: EnvTxBuilders api
    , envServerIdle            :: EnvServerIdle
    , envProcessRequest        :: EnvProcessRequest api
    }

serverTrackedAddresses :: EnvTrackedAddresses
serverTrackedAddresses = join $ asks envTrackedAddresses

defaultServerTrackedAddresses :: EnvTrackedAddresses
defaultServerTrackedAddresses = (:[]) <$> getWalletAddr

txEndpointsTxBuilders :: EnvTxBuilders api
txEndpointsTxBuilders input = asks envTxEndpointsTxBuilders >>= ($ input)

serverIdle :: EnvServerIdle
serverIdle = join $ asks envServerIdle

getQueueRef :: MonadReader (Env api) m => m (QueueRef api)
getQueueRef = asks envQueueRef

getNetworkId :: MonadReader (Env api) m => m NetworkId
getNetworkId = asks $ pNetworkId . envLedgerParams

instance (MonadIO m, MonadThrow m) => HasWallet (ReaderT (Env e) m) where
    getRestoredWallet = asks envWallet <&> fromMaybe (throw NoWalletProvided)

instance MonadIO m => HasChainIndex (ReaderT (Env e) m) where
    getChainIndex = asks envChainIndex

newtype NetworkM api a = NetworkM { unNetworkM :: ReaderT (Env api) Handler a }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader (Env api)
        , MonadError Servant.ServerError
        , MonadThrow
        , MonadCatch
        , HasWallet
        , HasChainIndex
        )

-- instance HasAssociatedApi (NetworkM api) where
--     type AssociatedApi (NetworkM api) = api

instance HasLogger (NetworkM s) where
    loggerFilePath = "server.log"

instance BF.HasBlockfrost (NetworkM s) where
  getBfToken = asks envBfToken
  getNetworkId = getNetworkId

loadEnv :: forall api. ()
    => ChainIndex
    -> EnvTrackedAddresses
    -> EnvTxBuilders api
    -> EnvServerIdle
    -> EnvProcessRequest api
    -> IO (Env api)
loadEnv defaultCI envTrackedAddresses envTxEndpointsTxBuilders envServerIdle envProcessRequest = do
    Config{..}   <- loadConfig
    envQueueRef  <- newIORef empty
    envWallet    <- sequence $ decodeOrErrorFromFile <$> cWalletFile
    -- envAuxiliary <- loadAuxiliaryEnv @s cAuxiliaryEnvFile
    pp           <- decodeOrErrorFromFile "protocol-parameters.json"
    let envMinUtxosNumber    = cMinUtxosNumber
        envMaxUtxosNumber    = cMaxUtxosNumber
        envLedgerParams      = Params def (pParamsFromProtocolParams pp) cNetworkId
        envInactiveEndpoints = cInactiveEndpoints
        envCollateral        = cCollateral
        envNodeFilePath      = cNodeFilePath
        envChainIndex        = fromMaybe defaultCI cChainIndex
        envBfToken           = cBfToken
    print cBfToken
    pure Env{..}

newtype AppM api a = AppM { unAppM :: ReaderT (Env api) IO a }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader (Env api)
        , MonadThrow
        , MonadCatch
        , HasWallet
        , HasChainIndex
        )

-- instance HasAssociatedApi (AppM api) where
--     type AssociatedApi (AppM api) = api

runAppM :: Env api -> AppM api a -> IO a
runAppM env app = runReaderT (unAppM app) env

instance HasLogger (AppM s) where
    loggerFilePath = "server.log"

instance BF.HasBlockfrost (AppM api) where
  getBfToken = asks envBfToken
  getNetworkId = getNetworkId

newtype QueueM api a = QueueM { unQueueM :: ReaderT (Env api) IO a }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader (Env api)
        , MonadThrow
        , MonadCatch
        , HasWallet
        , HasChainIndex
        )

instance HasLogger (QueueM s) where
    loggerFilePath = "queue.log"

checkEndpointAvailability :: (InactiveEndpoints -> Bool) -> NetworkM s ()
checkEndpointAvailability endpoint = whenM (asks (endpoint . envInactiveEndpoints)) $ throwM err404

type family AssociatedApi m where
    AssociatedApi (NetworkM api) = api
    AssociatedApi (AppM     api) = api
    AssociatedApi (QueueM   api) = api