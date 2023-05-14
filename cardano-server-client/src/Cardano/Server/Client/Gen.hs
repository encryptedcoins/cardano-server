{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Cardano.Server.Client.Gen where

import           Cardano.Api                        (BabbageEra, EraInMode (BabbageEraInCardanoMode),
                                                     SerialiseAsCBOR (serialiseToCBOR), Tx)
import           Cardano.Crypto.DSIGN.Class         (DSIGNAlgorithm (..), seedSizeDSIGN)
import           Cardano.Crypto.DSIGN.Ed25519       (Ed25519DSIGN)
import           Cardano.Node.Emulator.Generators   (genSomeCardanoApiTx)
import           Cardano.Server.Client.Internal     (ClientEndpoint (EndpointArg))
import           Cardano.Server.Config              (ServerEndpoint (FundsE, SubmitTxE))
import           Cardano.Server.Endpoints.Funds     (FundsReqBody (FundsReqBody))
import           Cardano.Server.Endpoints.Tx.Submit (SubmitTxReqBody (..))
import           Control.Monad                      (replicateM)
import           Control.Monad.IO.Class             (MonadIO (..))
import           Data.Aeson.Extras                  (encodeByteString)
import           Data.Data                          (Proxy)
import           Data.Functor                       ((<&>))
import           Data.Proxy                         (Proxy (..))
import           Data.Text                          (Text)
import           Hedgehog.Gen                       (sample)
import           Ledger                             (NetworkId, SomeCardanoApiTx (SomeTx))
import           Plutus.PAB.Arbitrary               ()
import           Plutus.V1.Ledger.Api               (CurrencySymbol (..), fromBuiltin)
import           PlutusAppsExtra.Utils.Address      (addressToBech32)
import           System.Random                      (randomRIO)
import           Test.Crypto.Util                   (Message, arbitrarySeedOfSize)
import           Test.QuickCheck                    (Arbitrary (..), generate)
import           Text.Hex                           (encodeHex)

randomFundsReqBody :: MonadIO m => NetworkId -> m (EndpointArg 'FundsE api)
randomFundsReqBody network = liftIO $ FundsReqBody
    <$> randomAddressBech32Text network
    <*> randomCSText

randomSubmitTxBody :: MonadIO m => m (EndpointArg 'SubmitTxE api)
randomSubmitTxBody = liftIO $ SubmitTxReqBody
    <$> randomCardanoTxText
    <*> (randomRIO (1,1) >>= (`replicateM` ((,) <$> randomPubKeyText <*> randomSignatureText)))

randomAddressBech32Text :: NetworkId -> IO Text
randomAddressBech32Text network = do
    generate arbitrary >>= maybe (randomAddressBech32Text network) pure . addressToBech32 network

randomCardanoTxText :: IO Text
randomCardanoTxText = randomBabbageEraTx <&> encodeByteString . serialiseToCBOR

randomCSText :: IO Text
randomCSText = do
    CurrencySymbol cs <- generate arbitrary
    pure $ encodeHex $ fromBuiltin cs

randomSignatureText :: IO Text
randomSignatureText = do
    msg :: Message <- generate arbitrary
    sig :: SigDSIGN Ed25519DSIGN <- generate $ signDSIGN () msg 
        <$> (genKeyDSIGN <$> arbitrarySeedOfSize (seedSizeDSIGN (Proxy :: Proxy Ed25519DSIGN)))
    pure $ encodeHex $ rawSerialiseSigDSIGN sig

randomPubKeyText :: IO Text
randomPubKeyText = do
    vk <- generate $ deriveVerKeyDSIGN @Ed25519DSIGN . genKeyDSIGN 
        <$> arbitrarySeedOfSize (seedSizeDSIGN (Proxy :: Proxy Ed25519DSIGN))
    pure $ encodeHex $ rawSerialiseVerKeyDSIGN vk

randomBabbageEraTx :: IO (Tx BabbageEra)
randomBabbageEraTx = sample genSomeCardanoApiTx >>= \case
    SomeTx tx BabbageEraInCardanoMode -> pure tx
    _                                 -> randomBabbageEraTx