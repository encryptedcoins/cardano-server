{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Server.Endpoints.FundsSpec where

import           Cardano.Server.Client.Gen      (randomFundsReqBody)
import           Cardano.Server.Client.Handle   (HasServantClientEnv)
import           Cardano.Server.Client.Internal (fundsC)
import           Cardano.Server.Config          (Config (..), loadConfig)
import           Cardano.Server.Test.Gen        (Malformed (..))
import           Cardano.Server.Test.Internal   (shoudlFailWithStatus, shouldBeOk)
import           Test.Hspec                     (Spec, describe, it)
import           Test.Hspec.Wai.QuickCheck      (Arbitrary (arbitrary), generate)

spec :: HasServantClientEnv => Spec
spec = describe "/funds" $ do

    it "get funds when all is ok" $ do
        Config{..} <- loadConfig
        fundsReqBody <- randomFundsReqBody cNetworkId
        shouldBeOk $ fundsC fundsReqBody

    it "fails with malformed request body" $ do
        Malformed fundsReqBody <- generate arbitrary
        fundsC fundsReqBody `shoudlFailWithStatus` 400