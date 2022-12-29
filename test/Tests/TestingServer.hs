{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Tests.TestingServer where

import           Control.Monad.IO.Class     (MonadIO(..))
import qualified Data.Map                   as Map
import           IO.ChainIndex              (getUtxosAt)
import           IO.Wallet                  (getWalletAddr)
import           PlutusTx.Builtins.Class    (stringToBuiltinByteString)
import           Server.Endpoints.Tx.Submit (processTokens)
import           Server.Internal            (HasServer(..))
import           Server.Tx                  (mkWalletTxOutRefs) 
import           Tests.Internal             (runTestM, testFunds, testFundsAll)
import           TestingServer.Main         (TestingServer)
import           Utils.ChainIndex           (filterCleanUtxos)
import           Utils.Logger               (HasLogger(..), logSmth, (.<))


testFundsTS :: IO ()
testFundsTS = testFunds @TestingServer

testFundsAllTS :: IO ()
testFundsAllTS = testFundsAll @TestingServer

testSubmitTxTS :: [String] -> IO ()
testSubmitTxTS = runTestM @TestingServer . processTokens . map stringToBuiltinByteString

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