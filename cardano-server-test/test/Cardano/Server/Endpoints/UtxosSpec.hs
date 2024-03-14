{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Server.Endpoints.UtxosSpec where

-- import           Cardano.Server.Client.Gen      (randomAddressBech32Text)
-- import           Cardano.Server.Client.Handle   (HasServantClientEnv)
-- import           Cardano.Server.Config          (decodeOrErrorFromFile)
-- import           Cardano.Server.Endpoints.Utxos (UtxosApi)
-- import           Cardano.Server.Example.Main    (ExampleApiConfig (..))
-- import           Cardano.Server.Test.Gen        (Malformed (..), malformedAddressTxt)
-- import           Cardano.Server.Test.Internal   (shoudlFailWithStatus, shouldBeOk)
-- import           Control.Monad.IO.Class         (MonadIO (..))
-- import           Servant                        (Proxy (..))
-- import           Servant.Client                 (client)
-- import           Test.Hspec                     (Spec, describe, it)

-- spec :: HasServantClientEnv => Spec
-- spec = describe "/utxos" $ do

--     it "get utxos when all is ok" $ do
--         ExampleApiConfig{..} <- decodeOrErrorFromFile "cardano-server-test/test/configuration/config.json"
--         addr <- randomAddressBech32Text cNetworkId
--         shouldBeOk $ client (Proxy @UtxosApi)  addr

--     it "fails with malformed request body" $ do
--         Malformed addrTxt <- liftIO malformedAddressTxt
--         client (Proxy @UtxosApi) addrTxt `shoudlFailWithStatus` 400