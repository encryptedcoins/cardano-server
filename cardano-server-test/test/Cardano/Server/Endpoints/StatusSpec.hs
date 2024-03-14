{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Server.Endpoints.StatusSpec where

import           Cardano.Server.Client.Client (HasServantClientEnv)
import           Cardano.Server.Error         (IsCardanoServerError (..))
import           Cardano.Server.Error.Servant (Envelope)
import           Cardano.Server.Example.Main  (ExampleStatusEndpointError (..), StatusApi)
import           Cardano.Server.Test.Internal (shoudlFailWithMessage, shoudlFailWithStatus, shouldBeOk)
import           Data.Text                    (Text)
import           Servant                      (Proxy (..))
import           Servant.Client               (ClientM, client)
import           Test.Hspec                   (Spec, describe, it)

spec :: HasServantClientEnv => Spec
spec = describe "/status" $ do

    it "gets status endpoint result when all is ok" $ do
        shouldBeOk $ client (Proxy @StatusApi) True

    it "gets corresponding error on failed request" $ do
        client (Proxy @StatusApi) False `shoudlFailWithStatus` fromEnum (errStatus ExampleStatusEndpointError)
        client (Proxy @StatusApi) False `shoudlFailWithMessage` errMsg ExampleStatusEndpointError

statusClient :: Bool -> ClientM (Envelope '[ExampleStatusEndpointError] Text)
statusClient = client (Proxy @StatusApi)