{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Test.TestingServer where

import           Control.Monad.IO.Class  (MonadIO(..))
import qualified Data.Map                as Map
import           IO.ChainIndex           (getUtxosAt)
import           IO.Wallet               (getWalletAddr)
import           PlutusTx.Builtins.Class (stringToBuiltinByteString)
import           Server.Internal         (HasServer(..))
import           Server.Tx               (mkWalletTxOutRefs) 
import           Test.Internal           (runTestM, testBalance, testBalanceAll)
import           TestingServer.Main      (TestingServer)
import           Utils.ChainIndex        (filterCleanUtxos)
import           Utils.Logger            (HasLogger(..), logSmth, (.<))

testBalanceTS :: IO ()
testBalanceTS = testBalance @TestingServer

testBalanceAllTS :: IO ()
testBalanceAllTS = testBalanceAll @TestingServer

testMintTS :: [String] -> IO ()
testMintTS = runTestM @TestingServer . processTokens . map stringToBuiltinByteString

mkRefs :: Int -> IO ()
mkRefs n = runTestM @TestingServer $ do
    addr  <- getWalletAddr
    utxos <- liftIO (getUtxosAt addr)
    refs   <- mkWalletTxOutRefs addr n
    utxos' <- liftIO $ getUtxosAt addr
    logMsg $ "Past utxo's ammount: "          .< Map.size utxos
    logMsg $ "Past clean utxo's ammount: "    .< Map.size (filterCleanUtxos utxos)
    logMsg $ "Current utxo's ammount: "       .< Map.size utxos'
    logMsg $ "Current clean utxo's ammount: " .< Map.size (filterCleanUtxos utxos')
    logSmth refs