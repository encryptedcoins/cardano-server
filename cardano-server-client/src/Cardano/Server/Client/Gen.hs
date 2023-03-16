{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.Server.Client.Gen where

import           Cardano.Api                        (BabbageEra, EraInMode (BabbageEraInCardanoMode),
                                                     SerialiseAsCBOR (serialiseToCBOR), Tx)
import           Cardano.Node.Emulator.Generators   (genSomeCardanoApiTx)
import           Cardano.Server.Client.Internal     (ClientEndpoint (EndpointArg), ServerEndpoint (FundsE, SubmitTxE))
import           Cardano.Server.Endpoints.Funds     (FundsReqBody (FundsReqBody))
import           Cardano.Server.Endpoints.Tx.Submit (SubmitTxReqBody (..))
import           Control.Monad                      (replicateM)
import           Control.Monad.IO.Class             (MonadIO (..))
import           Data.Aeson.Extras                  (encodeByteString)
import           Data.Functor                       ((<&>))
import           Data.Text                          (Text)
import           Hedgehog.Gen                       (sample)
import           Ledger                             (NetworkId, PubKey (PubKey), Signature (Signature), SomeCardanoApiTx (SomeTx))
import           Plutus.PAB.Arbitrary               ()
import           Plutus.V1.Ledger.Api               (CurrencySymbol (..), fromBuiltin)
import           Plutus.V1.Ledger.Bytes             (bytes)
import           PlutusAppsExtra.Utils.Address      (addressToBech32)
import           System.Random                      (randomRIO)
import           Test.QuickCheck                    (Arbitrary (..), generate)
import           Text.Hex                           (encodeHex)

randomFundsReqBody :: MonadIO m => NetworkId -> m (EndpointArg 'FundsE api)
randomFundsReqBody network = liftIO $ FundsReqBody
    <$> randomAddressBech32 network
    <*> randomCSText

randomSubmitTxBody :: MonadIO m => m (EndpointArg 'SubmitTxE api)
randomSubmitTxBody = liftIO $ SubmitTxReqBody
    <$> randomCardanoTxText
    <*> (randomRIO (1,10) >>= (`replicateM` ((,) <$> randomPubKeyText <*> randomSignatureText)))

randomAddressBech32 :: NetworkId -> IO Text
randomAddressBech32 network = do
    generate arbitrary >>= maybe (randomAddressBech32 network) pure . addressToBech32 network

randomCardanoTxText :: IO Text
randomCardanoTxText = randomBabbageEraTx <&> encodeByteString . serialiseToCBOR

randomCSText :: IO Text
randomCSText = do
    CurrencySymbol cs <- generate arbitrary
    pure $ encodeHex $ fromBuiltin cs

randomPubKeyText :: IO Text
randomPubKeyText = do
    Signature bs <- generate arbitrary
    pure $ encodeHex $ fromBuiltin bs

randomSignatureText :: IO Text
randomSignatureText = do
    PubKey bs <- generate arbitrary
    pure $ encodeHex $ bytes bs

randomBabbageEraTx :: IO (Tx BabbageEra)
randomBabbageEraTx = sample genSomeCardanoApiTx >>= \case
    SomeTx tx BabbageEraInCardanoMode -> pure tx
    _                                 -> randomBabbageEraTx
