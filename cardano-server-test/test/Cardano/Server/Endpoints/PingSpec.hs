{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Server.Endpoints.PingSpec where

import           Cardano.Server.Client.Handle   (HasServantClientEnv)
import           Cardano.Server.Client.Internal (pingC)
import           Cardano.Server.Test.Internal   (shouldBeOk)
import           Test.Hspec                     (Spec, describe, it)

spec :: HasServantClientEnv => Spec
spec = describe "/ping" $ do

        it "should always be available" $ do
            shouldBeOk pingC