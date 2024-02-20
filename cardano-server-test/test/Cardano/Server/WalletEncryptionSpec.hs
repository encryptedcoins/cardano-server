{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# OPTIONS_GHC -Wno-orphans   #-}

module Cardano.Server.WalletEncryptionSpec where

import           Cardano.Mnemonic                    (SomeMnemonic (SomeMnemonic))
import           Cardano.Server.WalletEncryption     (EncryptedWallet (..), decryptWallet, encryptWallet, genRandomIV,
                                                      mnemonicToBytes)
import           Cardano.Wallet.Gen                  (genMnemonic)
import           Cardano.Wallet.Primitive.Passphrase (Passphrase (Passphrase))
import           Control.Monad                       (join, replicateM)
import           Crypto.Cipher.AES                   (AES256)
import           Crypto.Cipher.Types                 (IV)
import           Data.Aeson                          (ToJSON (..), fromJSON)
import           Data.Bifunctor                      (Bifunctor (..))
import qualified Data.ByteArray                      as BA
import qualified Data.ByteString                     as BS
import           Data.Functor                        ((<&>))
import qualified Data.Text                           as T
import           Data.Text.Class                     (ToText (toText))
import           PlutusAppsExtra.IO.Wallet.Internal  (RestoredWallet (..))
import           Test.Hspec                          (Expectation, Spec, describe, it, shouldBe)
import           Test.QuickCheck                     (Arbitrary (arbitrary), property)
import           Test.QuickCheck.Utf8                (genUtf8Character)

spec :: Spec
spec = describe "Wallet encryption" $ do
    it "fromJSON . toJSON == id" $ property propJSON
    it "decrypt . encrypt == id" $ property encryptRoundTrip

propJSON :: EncryptedWallet -> Expectation
propJSON ew = fromJSON (toJSON ew) `shouldBe` pure ew

encryptRoundTrip :: RestoredWallet -> Expectation
encryptRoundTrip w@RestoredWallet{..} = do
    res <- fmap join $ encryptWallet w <&> second (`decryptWallet` toText passphrase)
    res`shouldBe` Right w

instance Arbitrary EncryptedWallet where
    arbitrary = do
        ewName     <- T.pack <$> arbitrary
        ewIVAES256 <- arbitrary
        ewMnemonic <- mnemonicToBytes . SomeMnemonic <$> genMnemonic @24
        pure EncryptedWallet{..}

instance Arbitrary (IV AES256) where
    arbitrary = genRandomIV (undefined :: AES256) >>= maybe arbitrary pure

instance Arbitrary RestoredWallet where
    arbitrary = do
        name             <- T.pack <$> arbitrary
        mnemonicSentence <- SomeMnemonic <$> genMnemonic @24
        passphrase       <- arbitrary
        pure RestoredWallet{..}

instance Arbitrary (Passphrase "user") where
    arbitrary = do
        l <- min 10 . max 100 <$> arbitrary
        Passphrase . BA.convert . BS.concat <$> replicateM l genUtf8Character