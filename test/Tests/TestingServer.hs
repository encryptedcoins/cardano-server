{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TupleSections     #-}

module Tests.TestingServer where

import           Cardano.Server.Endpoints.Tx.Server   (processInputs)
import           Cardano.Server.Internal              (HasServer(..), runAppM)
import           Cardano.Server.TestingServer.Main    (TestingServer)
import           Cardano.Server.Tx                    (mkWalletTxOutRefs)
import           Cardano.Server.Utils.Logger          (HasLogger(..), logSmth, (.<))
import           Control.Monad.IO.Class               (MonadIO(..))
import           Data.Default                         (def)
import qualified Data.Map                             as Map
import           IO.ChainIndex                        (getUtxosAt)
import           IO.Wallet                            (getWalletAddr, getWalletUtxos)
import           PlutusTx.Builtins.Class              (stringToBuiltinByteString)
import           Tests.DefaultClient                  (testDefaultClient)
import           Tests.Internal                       (testFunds, testFundsAll)
import           Utils.ChainIndex                     (filterCleanUtxos)

testFundsTS :: IO ()
testFundsTS = testFunds @TestingServer

testFundsAllTS :: IO ()
testFundsAllTS = testFundsAll @TestingServer

testSubmitTxTS :: [String] -> IO ()
testSubmitTxTS = runAppM @TestingServer . processInputs . (,def) . map stringToBuiltinByteString

testDefaultClientTS :: IO ()
testDefaultClientTS = testDefaultClient @TestingServer

mkRefs :: Int -> IO ()
mkRefs n = runAppM @TestingServer $ do
    addr   <- getWalletAddr
    utxos  <- getWalletUtxos
    refs   <- mkWalletTxOutRefs addr n
    utxos' <- getWalletUtxos
    logMsg $ "Past utxo's ammount: "          .< Map.size utxos
    logMsg $ "Past clean utxo's ammount: "    .< Map.size (filterCleanUtxos utxos)
    logMsg $ "Current utxo's ammount: "       .< Map.size utxos'
    logMsg $ "Current clean utxo's ammount: " .< Map.size (filterCleanUtxos utxos')
    logSmth refs