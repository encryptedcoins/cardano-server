{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE RankNTypes        #-}
{-# OPTIONS_GHC -Wno-orphans   #-}

module Cardano.Server.Test.Internal where

import           Cardano.Server.Client.Client  (createServantClientEnv)
import           Cardano.Server.Client.Handle  (HasServantClientEnv)
import           Cardano.Server.Config         (Config (cHyperTextProtocol, cWalletFile), decodeOrErrorFromFile)
import           Cardano.Server.Error          (parseErrorText)
import           Cardano.Server.Internal       (ServerHandle, envLogger, loadEnv)
import           Cardano.Server.Main           (ServerConstraints, runServer')
import           Cardano.Server.Utils.Logger   (mutedLogger)
import           Cardano.Server.Utils.Wait     (waitTime)
import qualified Control.Concurrent            as C
import           Control.Exception             (bracket)
import           Control.Monad                 (unless)
import           Control.Monad.IO.Class        (MonadIO (..))
import           Control.Monad.Reader          (ReaderT (runReaderT), ask)
import           Data.Aeson                    (decode)
import           Data.Either                   (isRight)
import           Data.Maybe                    (fromJust)
import           Data.Text                     (Text)
import           PlutusAppsExtra.IO.ChainIndex (ChainIndex (Kupo), HasChainIndex (..))
import           PlutusAppsExtra.IO.Wallet     (HasWallet (..), RestoredWallet, getWalletAda, restoreWalletFromFile)
import           Servant.Client                (ClientError (FailureResponse), ClientM,
                                                ResponseF (responseBody, responseStatusCode), runClientM)
import           Test.Hspec                    (Expectation, Spec, expectationFailure, hspec, it, shouldBe, shouldSatisfy)

withCardanoServer :: ServerConstraints api => FilePath -> ServerHandle api -> Integer -> (HasServantClientEnv => Spec) -> IO ()
withCardanoServer configFp sHandle minAdaInWallet specs = do
    config <- decodeOrErrorFromFile configFp
    env <- loadEnv config sHandle
    sce <- createServantClientEnv config
    let ?protocol = cHyperTextProtocol config
        ?servantClientEnv = sce
    walletHasEnouhgAda <- checkWalletHasMinAda $ fromJust $ cWalletFile config
    bracket
        (liftIO $ C.forkIO $ runServer' env{envLogger = mutedLogger})
        C.killThread $
        const $ (waitTime 5 >>) $ hspec $ do
            specs
            unless walletHasEnouhgAda
                $ it                 "The wallet has no minimum amount of ada."
                $ expectationFailure "This may cause some tests to fail. Please replenish your wallet and re-run the tests."
    where
        checkWalletHasMinAda fp = do
            w <- restoreWalletFromFile fp
            (>= fromInteger minAdaInWallet) <$> runReaderT getWalletAda w

instance HasChainIndex (ReaderT RestoredWallet IO) where
    getChainIndex = pure Kupo

instance HasWallet (ReaderT RestoredWallet IO) where
    getRestoredWallet = ask


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