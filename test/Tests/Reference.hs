{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE OverloadedStrings    #-}

module Tests.Reference where

import           Cardano.Server.Input                   (inputUTXO, InputContext (InputContextClient))
import           Cardano.Server.Internal                (runAppM)
import           Cardano.Server.TestingServer.Main      (TestingServer)
import           Cardano.Server.TestingServer.OffChain  (testToken)
import           Cardano.Server.TestingServer.OnChain   (testPolicyV, testPolicy)
import           Cardano.Server.Tx                      (mkTx)
import           Cardano.Server.Utils.Logger            (HasLogger(..))
import           Constraints.OffChain                   (postMintingPolicyTx, tokensMintedTx)
import           Control.Monad                          (void)
import           Control.Monad.Cont                     (MonadIO(..))
import           Data.Default                           (def)
import qualified Data.Map                               as Map
import           IO.Wallet                              (getWalletAddr, getWalletUtxos)
import           Ledger                                 (CardanoTx, unspentOutputsTx)
import qualified Ledger.Ada                             as Ada
import           Ledger.Tx                              (CardanoTx(..))
import           Ledger.Tx.CardanoAPI                   as CardanoAPI
import           Ledger.Typed.Scripts                   (Any)
import qualified PlutusTx.Prelude                       as Plutus

postReferenceScript :: IO CardanoTx
postReferenceScript = runAppM @TestingServer $ do
    addr <- getWalletAddr
    mkTx [] def
        [ postMintingPolicyTx
            addr
            testPolicyV
            (Nothing :: Maybe ())
            (Ada.adaValueOf 0)
        ]

runReferenceTest :: IO ()
runReferenceTest = void $ runAppM @TestingServer $ do
    ctx <- liftIO postReferenceScript
    let ref = head $ case ctx of
            EmulatorTx tx   -> Map.keys $ Ledger.unspentOutputsTx tx
            CardanoApiTx tx -> Map.keys $ CardanoAPI.unspentOutputsTx tx
    
    addr  <- getWalletAddr
    utxos <- getWalletUtxos
    let context = InputContextClient utxos utxos (head $ Map.keys utxos) addr
    logMsg "\n\n\n\t\t\tMINT1:"
    mkTest "token1" ref context
    
    addr'  <- getWalletAddr
    utxos' <- getWalletUtxos
    let context' = InputContextClient utxos' utxos' (head $ Map.keys utxos') addr'
    logMsg "\n\n\n\t\t\tMINT2:"
    mkTest "token2" ref context'
  where
    mkTest token ref context = mkTx
        []
        context
        [
            tokensMintedTx
            testPolicyV
            ([token] :: [Plutus.BuiltinByteString])
            (Plutus.sum $ map testToken [token])
        ]