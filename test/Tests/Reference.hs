{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE ImplicitParams       #-}
{-# LANGUAGE OverloadedStrings    #-}

module Tests.Reference where

import           Cardano.Server.Internal                (runAppM)
import           Cardano.Server.TestingServer.Main      (TestingServer)
import           Cardano.Server.TestingServer.OffChain  (testToken)
import           Cardano.Server.TestingServer.OnChain   (testPolicyV, testPolicy)
import           Cardano.Server.Tx                      (mkTx)
import           Cardano.Server.Utils.Logger            (HasLogger(..))
import           Constraints.OffChain                   (postMintingPolicyTx, referenceMintingPolicyTx)
import           Control.Monad                          (void)
import qualified Data.Map                               as Map
import           IO.Wallet                              (getWalletAddr)
import qualified Ledger.Ada                             as Ada
import           Ledger.Typed.Scripts                   (Any)
import qualified PlutusTx.Prelude                       as Plutus

postReferenceScript :: IO ()
postReferenceScript = void $ runAppM @TestingServer $ do
    addr <- getWalletAddr
    mkTx @Any [addr] Map.empty
        [ postMintingPolicyTx 
            ?txWalletAddr 
            testPolicyV 
            (Nothing :: Maybe ())
            (Ada.adaValueOf 20)
        ]

runReferenceTest :: IO ()
runReferenceTest = void $ runAppM @TestingServer $ do
    addr <- getWalletAddr
    logMsg "\n\n\n\t\t\tMINT1:"
    mkTest "token1" addr
    logMsg "\n\n\n\t\t\tMINT2:"
    mkTest "token2" addr
  where
    mkTest token addr = mkTx @Any [addr] Map.empty
        [ referenceMintingPolicyTx 
            testPolicy
            (head $ Map.keys ?txUtxos) 
            ([token] :: [Plutus.BuiltinByteString])
            (Plutus.sum $ map testToken [token])
        ]