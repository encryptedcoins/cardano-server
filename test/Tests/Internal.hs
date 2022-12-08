{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

module Tests.Internal where

import           Control.Monad            (unless, forM_)
import           Control.Monad.IO.Class   (MonadIO(..))
import           Control.Monad.Reader     (MonadReader, ReaderT(..), asks)
import           Data.Maybe               (fromJust)
import qualified Data.Text.IO             as T
import           IO.Wallet                (HasWallet(..), getWalletAddr, ownAddresses)
import           Ledger                   (Address)
import           Server.Endpoints.Funds   (getFunds, Funds(..))
import           Server.Internal          (HasServer(getCurrencySymbol), Env(..), loadEnv)
import           Utils.Address            (bech32ToAddress)
import           Utils.Logger             (HasLogger(..))

testFunds :: forall s. HasServer s => IO ()
testFunds = runTestM @s $ do
    addr <- getWalletAddr
    printFunds @s [addr]

testFundsAll :: forall s. HasServer s => IO ()
testFundsAll = runTestM @s $ ownAddresses >>= printFunds

printFunds :: forall s. HasServer s => [Address] -> TestM s ()
printFunds addreses = do
    cs <- getCurrencySymbol
    forM_ addreses $ \addr -> do
        Funds b <- getFunds cs addr
        unless (null b) $ liftIO $ print b

newtype TestM s a = TestM { unTestM :: ReaderT (Env s) IO a }
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader (Env s))

runTestM :: forall s a. HasServer s => TestM s a -> IO a
runTestM tm = do
    env <- loadEnv
    runReaderT (unTestM tm) env

instance HasLogger (TestM s) where
    loggerFilePath = ""

    logMsg = liftIO . T.putStrLn

instance HasWallet (TestM s) where
    getRestoreWallet = asks envWallet