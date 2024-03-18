{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Server.Test.Internal where

import           Cardano.Api                        (NetworkId (Testnet), NetworkMagic (NetworkMagic))
import           Cardano.Server.Client.Client       (HasServantClientEnv)
import           Cardano.Server.Config              (AuxillaryConfigOf, Config (..), decodeOrErrorFromFile)
import           Cardano.Server.Error.Class         (parseErrorText)
import           Cardano.Server.Internal            (AppT, AuxillaryEnvOf, ServerM, loadEnv, mkServantClientEnv)
import           Cardano.Server.Main                (IsCardanoServer, RunSettings (rsBeforeMainLoop), runServer)
import           Cardano.Server.Utils.Wait          (waitTime)
import qualified Control.Concurrent                 as C
import           Control.Exception                  (bracket)
import           Control.Monad                      (unless)
import           Control.Monad.IO.Class             (MonadIO (..))
import           Control.Monad.Reader               (ReaderT (runReaderT), ask)
import           Data.Aeson                         (decode)
import           Data.Default                       (Default (..))
import           Data.Either                        (isRight)
import           Data.Maybe                         (fromJust)
import           Data.Text                          (Text)
import           GHC.Records                        (HasField (..))
import           PlutusAppsExtra.IO.ChainIndex      (ChainIndexProvider (..), HasChainIndexProvider (..))
import qualified PlutusAppsExtra.IO.ChainIndex.Kupo as Kupo
import           PlutusAppsExtra.IO.Wallet          (HasWallet (..), HasWalletProvider (getWalletProvider), RestoredWallet,
                                                     WalletProvider (Cardano), getWalletAda, restoreWalletFromFile)
import           PlutusAppsExtra.Utils.Network      (HasNetworkId (..))
import           Servant                            (HasServer (..), ServerT)
import           Servant.Client                     (ClientError (FailureResponse), ClientM, ResponseF (responseBody, responseStatusCode),
                                                     runClientM)
import           Test.Hspec                         (Expectation, Spec, expectationFailure, hspec, it, shouldBe, shouldSatisfy)

withCardanoServer ::
    ( IsCardanoServer api
    , HasField "cWalletFile" (AuxillaryConfigOf  api) (Maybe FilePath)
    ) => FilePath -> ServerT api (ServerM api) -> AppT api IO () -> AuxillaryEnvOf api -> Integer -> (HasServantClientEnv => Spec) -> IO ()
withCardanoServer configFp server bml auxEnv minAdaInWallet specs = do
    config <- decodeOrErrorFromFile configFp
    let ?creds = Nothing
    env <- loadEnv config auxEnv
    sce <- mkServantClientEnv (cPort config) (cHost config) (cHyperTextProtocol config)
    let ?servantClientEnv = sce
    walletHasEnouhgAda <- checkWalletHasMinAda $ fromJust $ getField @"cWalletFile" $ cAuxilaryConfig config
    bracket
        (liftIO $ C.forkIO $ runServer server env def{rsBeforeMainLoop = bml})
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

instance HasNetworkId (ReaderT RestoredWallet IO) where
    getNetworkId = pure $ Testnet (NetworkMagic 1)

instance HasChainIndexProvider (ReaderT RestoredWallet IO) where
    getChainIndexProvider = pure Kupo
    getUtxosAt reqs addr = liftIO $ Kupo.getUtxosAt reqs addr
    getUnspentTxOutFromRef reqs txOutRef = liftIO $ Kupo.getUnspentTxOutFromRef reqs txOutRef

instance HasWallet (ReaderT RestoredWallet IO) where
    getRestoredWallet = ask

instance HasWalletProvider (ReaderT RestoredWallet IO) where
    getWalletProvider = pure Cardano

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