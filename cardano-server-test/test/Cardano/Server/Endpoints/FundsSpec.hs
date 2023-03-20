{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Server.Endpoints.FundsSpec where

import           Cardano.Server.Client.Gen      (randomFundsReqBody)
import           Cardano.Server.Client.Handle   (HasServantClientEnv)
import           Cardano.Server.Client.Internal (funds)
import           Cardano.Server.Config          (Config (..), loadConfig)
import           Test.Gen                       (Malformed (..))
import           Test.Hspec                     (Spec, describe, it)
import           Test.Hspec.Wai.QuickCheck      (Arbitrary (arbitrary), generate)
import           Test.Internal                  (shoudlFailWithStatus, shouldBeOk)

spec :: HasServantClientEnv => Spec
spec = describe "/funds" $ do

    it "get funds when all is ok" $ do
        Config{..} <- loadConfig
        fundsReqBody <- randomFundsReqBody cNetworkId
        shouldBeOk $ funds fundsReqBody

    it "fails with malformed request body" $ do
        Malformed fundsReqBody <- generate arbitrary
        funds fundsReqBody `shoudlFailWithStatus` 400