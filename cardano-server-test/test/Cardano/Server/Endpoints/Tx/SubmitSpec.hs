{-# LANGUAGE OverloadedStrings #-}

module Cardano.Server.Endpoints.Tx.SubmitSpec where
    
import           Cardano.Server.Client.Gen      (randomSubmitTxBody)
import           Cardano.Server.Client.Handle   (HasServantClientEnv)
import           Cardano.Server.Client.Internal (submitTxC)
import           Test.Gen                       (Malformed (..))
import           Test.Hspec                     (Spec, describe, it)
import           Test.Hspec.Wai.QuickCheck      (Arbitrary (arbitrary), generate)
import           Test.Internal                  (shoudlFailWithStatus, shouldBeOk)

spec :: HasServantClientEnv => Spec
spec = describe "/submitTx" $ do

    it "submits tx when all is ok" $ do
        input <- randomSubmitTxBody
        shouldBeOk $ submitTxC input
        
    it "fails with malformed request body" $ do
        Malformed submitTxBody <- generate arbitrary
        submitTxC submitTxBody `shoudlFailWithStatus` 400