{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Cardano.Server.Endpoints.Tx.ServerSpec where
    
import           Cardano.Server.Client.Example.Main (genInput)
import           Cardano.Server.Client.Handle       (HasServantClientEnv)
import           Cardano.Server.Client.Internal     (serverTx)
import           Cardano.Server.Example.Main        (ExampleApi)
import           Data.Bifunctor                     (Bifunctor (..))
import           Test.Hspec                         (Spec, describe, it)
import           Test.Internal                      (shoudlFailWithStatus, shouldBeOk)

spec :: HasServantClientEnv => Spec
spec = describe "/serverTx" $ do

    it "adds reqeust to queue when all is ok" $ do
        input <- genInput
        shouldBeOk $ serverTx @ExampleApi input
        
    it "fails when request contains duplicate tokens" $ do
        input <- first (("aaaa":) . ("aaaa":)) <$> genInput
        serverTx @ExampleApi input `shoudlFailWithStatus` 422