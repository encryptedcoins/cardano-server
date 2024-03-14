{-# LANGUAGE OverloadedStrings #-}

module Cardano.Server.Endpoints.Tx.SubmitSpec where

-- import           Cardano.Server.Client.Handle   (HasServantClientEnv)
-- import           Cardano.Server.Client.Internal (submitTxC)
-- import           Cardano.Server.Test.Gen        (Malformed (..))
-- import           Cardano.Server.Test.Internal   (shoudlFailWithStatus)
-- import           Test.Hspec                     (Spec, describe, it)
-- import           Test.Hspec.Wai.QuickCheck      (Arbitrary (arbitrary), generate)

-- spec :: HasServantClientEnv => Spec
-- spec = describe "/submitTx" $ do

--     it "fails with malformed request body" $ do
--         Malformed submitTxBody <- generate arbitrary
--         submitTxC submitTxBody `shoudlFailWithStatus` 400