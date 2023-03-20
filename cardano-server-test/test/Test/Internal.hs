{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase     #-}

module Test.Internal where

import           Cardano.Server.Client.Handle (HasServantClientEnv)
import           Data.Either                  (isRight)
import           Servant.Client               (ClientError (FailureResponse), ClientM, ResponseF (responseStatusCode), runClientM)
import           Test.Hspec                   (Expectation, expectationFailure, shouldBe, shouldSatisfy)

shoudlFailWithStatus :: (Show a, HasServantClientEnv) => ClientM a -> Int -> Expectation
shoudlFailWithStatus ma s = runClientM ma ?servantClientEnv >>= \case
    Left (FailureResponse _ resp) -> responseStatusCode resp `shouldBe` toEnum s
    x -> expectationFailure $ "Not a failure response:\n" <> show x

shouldBeOk :: (Show a, HasServantClientEnv) => ClientM a -> Expectation
shouldBeOk ma = runClientM ma ?servantClientEnv >>= (`shouldSatisfy` isRight)