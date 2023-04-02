{-# LANGUAGE TypeApplications #-}

module Cardano.Server.Endpoints.StatusSpec where

import           Cardano.Server.Client.Handle   (HasServantClientEnv)
import           Cardano.Server.Client.Internal (statusC)
import           Cardano.Server.Error           (IsCardanoServerError (..))
import           Cardano.Server.Example.Main    (ExampleApi, ExampleStatusEndpointError (..))
import           Test.Hspec                     (Spec, describe, it)
import           Test.Internal                  (shoudlFailWithMessage, shoudlFailWithStatus, shouldBeOk)

spec :: HasServantClientEnv => Spec
spec = describe "/status" $ do

    it "gets status endpoint result when all is ok" $ do
        shouldBeOk $ statusC @ExampleApi True

    it "gets corresponding error on failed request" $ do
        statusC @ExampleApi False `shoudlFailWithStatus` fromEnum (errStatus ExampleStatusEndpointError)
        statusC @ExampleApi False `shoudlFailWithMessage` errMsg ExampleStatusEndpointError