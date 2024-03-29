{-# LANGUAGE TypeApplications #-}

module Cardano.Server.Endpoints.StatusSpec where

import           Cardano.Server.Client.Handle (HasServantClientEnv)
import           Cardano.Server.Error         (IsCardanoServerError (..))
import           Cardano.Server.Example.Main  (ExampleStatusEndpointError (..), StatusApi)
import           Cardano.Server.Test.Internal (shoudlFailWithMessage, shoudlFailWithStatus, shouldBeOk)
import           Data.Data                    (Proxy (..))
import           Servant.Client               (client)
import           Test.Hspec                   (Spec, describe, it)

spec :: HasServantClientEnv => Spec
spec = describe "/status" $ do

    it "gets status endpoint result when all is ok" $ do
        shouldBeOk $ statusC True

    it "gets corresponding error on failed request" $ do
        statusC False `shoudlFailWithStatus` fromEnum (errStatus ExampleStatusEndpointError)
        statusC False `shoudlFailWithMessage` errMsg ExampleStatusEndpointError
  where
    statusC = client (Proxy @StatusApi)