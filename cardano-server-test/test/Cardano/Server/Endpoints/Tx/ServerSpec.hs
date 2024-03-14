{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Cardano.Server.Endpoints.Tx.ServerSpec where

-- import           Cardano.Server.Client.Example.Main (genInput)
-- import           Cardano.Server.Client.Handle       (HasServantClientEnv)
-- import           Cardano.Server.Client.Internal     (serverTxC)
-- import           Cardano.Server.Example.Main        (ExampleApi)
-- import           Cardano.Server.Test.Internal       (shoudlFailWithStatus, shouldBeOk)
-- import           Data.Bifunctor                     (Bifunctor (..))
-- import           Test.Hspec                         (Spec, describe, it)

-- spec :: HasServantClientEnv => Spec
-- spec = describe "/serverTx" $ do

--     it "adds reqeust to queue when all is ok" $ do
--         input <- genInput
--         shouldBeOk $ serverTxC @ExampleApi input

--     it "fails when request contains duplicate tokens" $ do
--         input <- first (("aaaa":) . ("aaaa":)) <$> genInput
--         serverTxC @ExampleApi input `shoudlFailWithStatus` 422