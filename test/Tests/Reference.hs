{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE ImplicitParams       #-}
{-# LANGUAGE OverloadedStrings    #-}

module Tests.Reference where

import           Control.Monad           (void)
import qualified Data.Map                as Map
import           IO.Wallet               (getWalletAddr)
import qualified Ledger.Ada              as Ada
import           Ledger.Typed.Scripts    (Any)
import           Constraints.OffChain    (postMintingPolicyTx, referenceMintingPolicyTx)
import           Server.Internal         (runAppM)
import           Server.Tx               (mkTx)
import           TestingServer.Main      (TestingServer)
import           TestingServer.OffChain  (testToken)
import           TestingServer.OnChain   (testPolicyV, testPolicy)
import qualified PlutusTx.Prelude        as Plutus
import           Utils.Logger            (HasLogger(..))

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