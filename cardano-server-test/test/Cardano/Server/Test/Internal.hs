{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams   #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-# LANGUAGE RankNTypes       #-}

module Cardano.Server.Test.Internal where

import           Cardano.Server.Client.Client (createServantClientEnv)
import           Cardano.Server.Client.Handle (HasServantClientEnv)
import           Cardano.Server.Error         (parseErrorText)
import           Cardano.Server.Internal      (ServerHandle, envLogger, loadEnv)
import           Cardano.Server.Main          (ServerConstraints, runServer')
import           Cardano.Server.Utils.Logger  (mutedLogger)
import           Cardano.Server.Utils.Wait    (waitTime)
import qualified Control.Concurrent           as C
import           Control.Exception            (bracket)
import           Control.Monad.IO.Class       (MonadIO (..))
import           Data.Aeson                   (decode)
import           Data.Either                  (isRight)
import           Data.Text                    (Text)
import           Servant.Client               (ClientError (FailureResponse), ClientM,
                                               ResponseF (responseBody, responseStatusCode), runClientM)
import           Test.Hspec                   (Expectation, Spec, expectationFailure, hspec, shouldBe, shouldSatisfy)

withCardanoServer :: ServerConstraints api => ServerHandle api -> (HasServantClientEnv => Spec) -> IO ()
withCardanoServer sHandle specs = do
    env <- loadEnv sHandle
    sce <- createServantClientEnv
    let ?servantClientEnv = sce
    bracket
        (liftIO $ C.forkIO $ runServer' env{envLogger = mutedLogger})
        C.killThread $
        const $ (waitTime 5 >>) $ hspec specs

shoudlFailWithStatus :: (Show a, HasServantClientEnv) => ClientM a -> Int -> Expectation
shoudlFailWithStatus ma s = runClientM ma ?servantClientEnv >>= \case
    Left (FailureResponse _ resp) -> responseStatusCode resp `shouldBe` toEnum s
    x                             -> expectationFailure $ "Not a failure response:\n" <> show x

shoudlFailWithMessage :: (Show a, HasServantClientEnv) => ClientM a -> Text -> Expectation
shoudlFailWithMessage ma msg = runClientM ma ?servantClientEnv >>= \case
    Left (FailureResponse _ resp) -> (decode (responseBody resp) >>= parseErrorText) `shouldBe` Just msg
    x                             -> expectationFailure $ "Not a failure response:\n" <> show x

shouldBeOk :: (Show a, HasServantClientEnv) => ClientM a -> Expectation
shouldBeOk ma = runClientM ma ?servantClientEnv >>= (`shouldSatisfy` isRight)