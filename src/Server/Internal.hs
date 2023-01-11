{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

module Server.Internal 
    ( module Server.Class
    , NetworkM(..)
    , AppM(..)
    , runAppM
    , getQueueRef
    , loadEnv
    ) where

import           Control.Monad.Catch    (MonadThrow, MonadCatch)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (ReaderT(ReaderT, runReaderT), MonadReader, asks)
import           Data.Default           (def)
import           Data.IORef             (newIORef)
import           Data.Sequence          (empty)
import           IO.Wallet              (HasWallet(..))
import           Ledger                 (Params(..), pParamsFromProtocolParams)
import           Servant                (Handler)
import           Server.Class           (Env(..), Queue, QueueRef, QueueElem, HasServer(..))
import           Server.Config          (Config(..), configFile, decodeOrErrorFromFile)
import           Utils.Logger           (HasLogger(..))

newtype NetworkM s a = NetworkM { unNetworkM :: ReaderT (Env s) Handler a }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader (Env s)
        , MonadThrow
        , MonadCatch
        , HasWallet
        )

instance HasLogger (NetworkM s) where
    loggerFilePath = "server.log"

getQueueRef :: NetworkM s (QueueRef s)
getQueueRef = asks envQueueRef

loadEnv :: forall s. HasServer s => IO (Env s)
loadEnv = do
    Config{..}   <- decodeOrErrorFromFile configFile
    envQueueRef  <- newIORef empty
    envWallet    <- decodeOrErrorFromFile cWalletFile
    envAuxiliary <- loadAuxiliaryEnv @s cAuxiliaryEnvFile
    pp           <- decodeOrErrorFromFile "testnet/protocol-parameters.json"
    let envMinUtxosAmount = cMinUtxosAmount
        envLedgerParams   = Params def (pParamsFromProtocolParams pp) cNetworkId
    pure Env{..}

newtype AppM s a = AppM { unAppM :: ReaderT (Env s) IO a }
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader (Env s))

runAppM :: HasServer s => AppM s a -> IO a
runAppM app = loadEnv >>= runReaderT (unAppM app)

instance HasLogger (AppM s) where
    loggerFilePath = "server.log"

instance HasWallet (AppM s) where
    getRestoredWallet = asks envWallet