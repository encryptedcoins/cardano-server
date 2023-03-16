{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Cardano.Server.Client.Gen where

import           Cardano.Node.Emulator.Generators   (genSomeCardanoApiTx)
import           Cardano.Server.Client.Internal     (ClientEndpoint (EndpointArg), ServerEndpoint (FundsE, SubmitTxE))
import           Cardano.Server.Endpoints.Funds     (FundsReqBody (FundsReqBody))
import           Cardano.Server.Endpoints.Tx.Submit (SubmitTxReqBody (..))
import           Control.Monad.IO.Class             (MonadIO (..))
import           Data.Bifunctor                     (Bifunctor (bimap))
import qualified Data.Text                          as T
import           Hedgehog.Gen                       (sample)
import           Ledger                             (Address, CardanoTx (CardanoApiTx), CurrencySymbol, PubKey, Signature, NetworkId)
import           Plutus.PAB.Arbitrary               ()
import           Test.QuickCheck                    (Arbitrary (..), generate)
import PlutusAppsExtra.Utils.Address (addressToBech32)
import Data.Text ( Text )

randomFundsReqBody :: MonadIO m => NetworkId -> m (EndpointArg 'FundsE api)
randomFundsReqBody network = liftIO $ FundsReqBody
    <$> randomAddressBech32 network
    <*> generate (T.pack . show <$> (arbitrary @CurrencySymbol))

randomAddressBech32 :: NetworkId -> IO Text
randomAddressBech32 network = do
    generate arbitrary >>= (. addressToBech32 network) \case
        Just res -> pure res
        Nothing  -> randomAddressBech32 network

randomSubmitTxBody :: MonadIO m => m (EndpointArg 'SubmitTxE api)
randomSubmitTxBody = liftIO $ SubmitTxReqBody
    <$> (T.pack . show <$> randomCardanoTx)
    <*> (fmap (bimap (T.pack . show) (T.pack . show)) <$> liftIO (generate $ arbitrary @[(PubKey, Signature)]))

randomCardanoTx :: IO CardanoTx
randomCardanoTx = sample $ CardanoApiTx <$> genSomeCardanoApiTx