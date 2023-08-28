{-# LANGUAGE FlexibleInstances #-}

module Cardano.Server.Test.Gen where

import           Cardano.Server.Client.Handle       ()
import           Cardano.Server.Endpoints.Tx.Submit (SubmitTxReqBody (SubmitTxReqBody), parseSubmitTxReqBody)
import           Control.Monad.IO.Class             (MonadIO (..))
import           Data.Text                          (Text)
import           PlutusAppsExtra.Utils.Address      (bech32ToAddress)
import           Test.QuickCheck                    (Arbitrary (arbitrary), generate)

newtype Malformed a = Malformed a

instance (Arbitrary (Malformed SubmitTxReqBody)) where
    arbitrary = do
        strb <- SubmitTxReqBody <$> arbitrary <*> arbitrary
        case parseSubmitTxReqBody strb of
            Right _ -> arbitrary
            Left _ -> pure $ Malformed strb

malformedAddressTxt :: MonadIO m => m (Malformed Text)
malformedAddressTxt = do
    txt <- liftIO $ generate arbitrary
    case bech32ToAddress txt of
        Just _  -> malformedAddressTxt
        Nothing -> pure $ Malformed txt