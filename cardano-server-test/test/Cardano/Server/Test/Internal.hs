{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Cardano.Server.Test.Internal where

import           Cardano.Server.Client.Handle (HasServantClientEnv)
import           Cardano.Server.Config        (CardanoServerConfig (..))
import           Cardano.Server.Error         (parseErrorText)
import           Cardano.Server.Internal      (mkServantClientEnv)
import           Cardano.Server.Main          (runCardanoServer)
import           Cardano.Server.Utils.Logger  (HasLogger)
import           Cardano.Server.Utils.Wait    (waitTime)
import qualified Control.Concurrent           as C
import           Control.Exception            (bracket)
import           Control.Monad.Catch          (MonadThrow (..))
import           Control.Monad.Extra          (ifM)
import           Control.Monad.IO.Class       (MonadIO (..))
import           Data.Aeson                   (decode)
import           Data.Either                  (isRight)
import           Data.Maybe                   (isJust)
import           Data.Text                    (Text)
import           PlutusAppsExtra.IO.Wallet    (HasWalletProvider, RestoredWallet, getWalletAda)
import           Servant                      (Handler, HasServer, ServerT, runHandler)
import           Servant.Client               (ClientError (FailureResponse), ClientM, ResponseF (responseBody, responseStatusCode),
                                               runClientM)
import           Test.Hspec                   (Expectation, Spec, expectationFailure, hspec, it, runIO, shouldBe, shouldSatisfy)
import PlutusAppsExtra.IO.ChainIndex (HasChainIndexProvider)

withCardanoServer :: forall api m c.
    ( HasServer api '[]
    , CardanoServerConfig c
    , HasWalletProvider m
    , HasChainIndexProvider m
    , HasLogger m
    )
    => c
    -> (forall a. m a -> Servant.Handler a)
    -> ServerT api m
    -> m ()
    -> Maybe RestoredWallet
    -> Integer
    -> (HasServantClientEnv => Spec)
    -> IO ()
withCardanoServer config runApp serverApp beforeMainLoop mbWallet minAdaInWallet specs = do
    let ?creds = Nothing
    sce <- mkServantClientEnv (configPort config) (configHost config) (configHyperTextProtocol config)
    let ?servantClientEnv = sce
    bracket
        (liftIO $ C.forkIO $ runCardanoServer @api config runApp serverApp beforeMainLoop)
        C.killThread $
        const $ (waitTime 5 >>) $ hspec specsWithMinAdaCheck
  where
    specsWithMinAdaCheck :: HasServantClientEnv => Spec
    specsWithMinAdaCheck
        | minAdaInWallet <= 0 = specs
        | isJust mbWallet = ifM (runIO walletHasMinAda) specs (specs >> noAdaWarn)
        | otherwise = specs >> noWalletWarn
    walletHasMinAda = fmap (>= fromInteger minAdaInWallet) $ either throwM pure =<< Servant.runHandler (runApp getWalletAda)
    noAdaWarn = it "The wallet has no minimum amount of ada." $
        expectationFailure "This may cause some tests to fail. Please replenish your wallet and re-run the tests."
    noWalletWarn = it "You didn't specify a wallet, although the tests require some amount of ada." $
        expectationFailure "This may cause some tests to fail. Please add wallet to configuration folder and re-run the tests."

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