{-# LANGUAGE RecordWildCards #-}

module Cardano.Server.Endpoints.UtxosSpec where

import           Cardano.Server.Client.Gen      (randomAddressBech32Text)
import           Cardano.Server.Client.Handle   (HasServantClientEnv)
import           Cardano.Server.Client.Internal (utxosC)
import           Cardano.Server.Config          (Config (..), decodeOrErrorFromFile)
import           Cardano.Server.Test.Gen        (Malformed (..), malformedAddressTxt)
import           Cardano.Server.Test.Internal   (shoudlFailWithStatus, shouldBeOk)
import           Control.Monad.IO.Class         (MonadIO (..))
import           Test.Hspec                     (Spec, describe, it)

spec :: HasServantClientEnv => Spec
spec = describe "/utxos" $ do

    it "get utxos when all is ok" $ do
        Config{..} <- decodeOrErrorFromFile "cardano-server-test/test/configuration/config.json"
        addr <- randomAddressBech32Text cNetworkId
        shouldBeOk $ utxosC addr

    it "fails with malformed request body" $ do
        Malformed addrTxt <- liftIO malformedAddressTxt
        utxosC addrTxt `shoudlFailWithStatus` 400