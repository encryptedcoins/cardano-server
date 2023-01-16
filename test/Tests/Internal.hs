{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

module Tests.Internal where

import           Cardano.Server.Endpoints.Funds        (getFunds, Funds(..))
import           Cardano.Server.Internal               (HasServer(..), Env(..), loadEnv, AppM, runAppM)
import           Cardano.Server.TestingServer.OffChain (testCurrencySymbol)
import           Cardano.Server.Utils.Logger           (HasLogger(..))
import           Control.Monad                         (unless, forM_)
import           Control.Monad.IO.Class                (MonadIO(..))
import           Control.Monad.Reader                  (MonadReader, ReaderT(..), asks)
import           Data.Maybe                            (fromJust)
import qualified Data.Text.IO                          as T
import           IO.Wallet                             (HasWallet(..), getWalletAddr, ownAddresses)
import           Ledger                                (Address)
import           Utils.Address                         (bech32ToAddress)

testFunds :: forall s. HasServer s => IO ()
testFunds = runAppM @s $ do
    addr <- getWalletAddr
    printFunds @s [addr]

testFundsAll :: forall s. HasServer s => IO ()
testFundsAll = runAppM @s $ ownAddresses >>= printFunds

printFunds :: forall s. HasServer s => [Address] -> AppM s ()
printFunds addreses = do
    let cs = testCurrencySymbol
    forM_ addreses $ \addr -> do
        Funds b <- getFunds cs addr
        unless (null b) $ liftIO $ print b