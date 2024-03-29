{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Cardano.Server.Endpoints.Tx.ServerSpec where

import           Cardano.Server.Internal
import           Cardano.Server.Client.Example.Main (genInput, mkCtx)
import           Cardano.Server.Client.Handle       (HasServantClientEnv)
import           Cardano.Server.Client.Internal     (serverTxC)
import           Cardano.Server.Example.Main        (ExampleApi)
import           Cardano.Server.Test.Internal       (shoudlFailWithStatus, shouldBeOk)
import           Data.Bifunctor                     (Bifunctor (..))
import           Test.Hspec                         (Spec, describe, it)

spec :: HasServantClientEnv => Env ExampleApi -> Spec
spec env = describe "/serverTx" $ do

    it "adds reqeust to queue when all is ok" $ do
        input <- runServerM env $ (,) <$> genInput <*> mkCtx
        shouldBeOk $ serverTxC @ExampleApi input

    it "fails when request contains duplicate tokens" $ do
        input <- runServerM env $ (,) <$> genInput <*> mkCtx
        let duplicateInput = first ((("aaaa", 1):) . (("aaaa", 2):)) input
        serverTxC @ExampleApi duplicateInput `shoudlFailWithStatus` 422