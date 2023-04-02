{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Server.Endpoints.PingSpec where

import           Cardano.Server.Client.Handle   (HasServantClientEnv)
import           Cardano.Server.Client.Internal (pingC)
import           Test.Hspec                     (Spec, describe, it)
import           Test.Internal                  (shouldBeOk)

spec :: HasServantClientEnv => Spec
spec = describe "/ping" $ do

        it "should always be available" $ do
            shouldBeOk pingC