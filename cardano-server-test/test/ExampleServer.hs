{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TupleSections     #-}

module ExampleServer where

import           Cardano.Server.Endpoints.Tx.Server   (processInputs)
import           Cardano.Server.Internal              (HasServer(..), runAppM)
import           Cardano.Server.Example.Main          (ExampleServer)
import           Cardano.Server.Tx                    (mkWalletTxOutRefs)
import           Cardano.Server.Utils.Logger          (HasLogger(..), logSmth, (.<))
import           Cardano.Server.Client.Example.Main   ()
import           Control.Monad.IO.Class               (MonadIO(..))
import           Data.Default                         (def)
import qualified Data.Map                             as Map
import           IO.ChainIndex                        (getUtxosAt)
import           IO.Wallet                            (getWalletAddr, getWalletUtxos)
import           PlutusTx.Builtins.Class              (stringToBuiltinByteString)
import           DefaultClient                        (testDefaultClient)
import           Internal                             (testFunds, testFundsAll)
import           Utils.ChainIndex                     (filterCleanUtxos)

testFundsExample :: IO ()
testFundsExample = testFunds @ExampleServer

testFundsAllExample :: IO ()
testFundsAllExample = testFundsAll @ExampleServer

testSubmitTxExample :: [String] -> IO ()
testSubmitTxExample = runAppM @ExampleServer . processInputs . (,def) . map stringToBuiltinByteString

testDefaultClientExample :: IO ()
testDefaultClientExample = testDefaultClient @ExampleServer

mkRefs :: Int -> IO ()
mkRefs n = runAppM @ExampleServer $ do
    addr   <- getWalletAddr
    utxos  <- getWalletUtxos
    refs   <- mkWalletTxOutRefs addr n
    utxos' <- getWalletUtxos
    logMsg $ "Past utxo's ammount: "          .< Map.size utxos
    logMsg $ "Past clean utxo's ammount: "    .< Map.size (filterCleanUtxos utxos)
    logMsg $ "Current utxo's ammount: "       .< Map.size utxos'
    logMsg $ "Current clean utxo's ammount: " .< Map.size (filterCleanUtxos utxos')
    logSmth refs