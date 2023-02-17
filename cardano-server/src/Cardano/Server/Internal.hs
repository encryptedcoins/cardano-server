{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

module Cardano.Server.Internal
    ( module Cardano.Server.Class
    , NetworkM (..)
    , AppM (..)
    , runAppM
    , getNetworkId
    , getQueueRef
    , loadEnv
    , checkEndpointAvailability
    ) where

import           Cardano.Node.Emulator           (Params (..), pParamsFromProtocolParams)
import           Cardano.Server.Class            (Env (..), HasServer (..), Queue, QueueRef)
import           Cardano.Server.Config           (Config (..), InactiveEndpoints, decodeOrErrorFromFile, loadConfig)
import           Cardano.Server.Utils.Logger     (HasLogger (..), logSmth)
import           Control.Monad.Catch             (Exception (..), MonadCatch, MonadThrow (..))
import           Control.Monad.Except            (throwError)
import           Control.Monad.Extra             (whenM)
import           Control.Monad.IO.Class          (MonadIO)
import           Control.Monad.Reader            (MonadReader, ReaderT (ReaderT, runReaderT), asks, lift)
import           Data.Default                    (def)
import           Data.IORef                      (newIORef)
import           Data.Maybe                      (fromMaybe)
import           Data.Sequence                   (empty)
import           Ledger                          (NetworkId)
import           PlutusAppsExtra.IO.ChainIndex   (HasChainIndex)
import           PlutusAppsExtra.IO.Wallet       (HasWallet (..))
import           Servant                         (Handler, err404)

newtype NetworkM s a = NetworkM { unNetworkM :: ReaderT (Env s) Handler a }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader (Env s)
        , MonadCatch
        , HasWallet
        , HasChainIndex
        )

-- Servant does not notice its own errors thrown through throwM
instance MonadThrow (NetworkM s) where
    throwM e = do
        logSmth e
        case fromException $ toException e of 
            Just servantError -> NetworkM . lift $ throwError servantError
            Nothing           -> NetworkM $ throwM e

instance HasLogger (NetworkM s) where
    loggerFilePath = "server.log"

getQueueRef :: NetworkM s (QueueRef s)
getQueueRef = asks envQueueRef

getNetworkId :: MonadReader (Env s) m => m NetworkId
getNetworkId = asks $ pNetworkId . envLedgerParams

loadEnv :: forall s. HasServer s => IO (Env s)
loadEnv = do
    Config{..}   <- loadConfig
    envQueueRef  <- newIORef empty
    envWallet    <- decodeOrErrorFromFile cWalletFile
    envAuxiliary <- loadAuxiliaryEnv @s cAuxiliaryEnvFile
    pp           <- decodeOrErrorFromFile "protocol-parameters.json"
    let envMinUtxosAmount = cMinUtxosAmount
        envLedgerParams   = Params def (pParamsFromProtocolParams pp) cNetworkId
        envInactiveEndpoints = cInactiveEndpoints
        envCollateral        = cCollateral
        envNodeFilePath      = cNodeFilePath
        envChainIndex        = fromMaybe (defaultChainIndex @s) cChainIndex
    pure Env{..}

newtype AppM s a = AppM { unAppM :: ReaderT (Env s) IO a }
    deriving newtype 
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader (Env s)
        , MonadThrow
        , MonadCatch
        , HasChainIndex
        )

runAppM :: HasServer s => AppM s a -> IO a
runAppM app = loadEnv >>= runReaderT (unAppM app)

instance HasLogger (AppM s) where
    loggerFilePath = "server.log"

instance HasWallet (AppM s) where
    getRestoredWallet = asks envWallet

checkEndpointAvailability :: (InactiveEndpoints -> Bool) -> NetworkM s ()
checkEndpointAvailability endpoint = whenM (asks (endpoint . envInactiveEndpoints)) $ throwM err404 