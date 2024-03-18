module Cardano.Server.Endpoints.PingSpec where

import           Cardano.Server.Client.Client  (HasServantClientEnv)
import           Cardano.Server.Endpoints.Ping (PingApi)
import           Cardano.Server.Test.Internal  (shouldBeOk)
import           Servant                       (Proxy (..))
import           Servant.Client                (client)
import           Test.Hspec                    (Spec, describe, it)

spec :: HasServantClientEnv => Spec
spec = describe "/ping" $ do

        it "should always be available" $ do
            shouldBeOk $ client (Proxy @PingApi)
