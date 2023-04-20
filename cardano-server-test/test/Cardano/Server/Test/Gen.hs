{-# LANGUAGE FlexibleInstances #-}

module Cardano.Server.Test.Gen where

import           Cardano.Server.Client.Handle       ()
import           Cardano.Server.Endpoints.Funds     (FundsReqBody (..), parseFundsReqBody)
import           Cardano.Server.Endpoints.Tx.Submit (SubmitTxReqBody (SubmitTxReqBody), parseSubmitTxReqBody)
import           Test.QuickCheck                    (Arbitrary (arbitrary))

newtype Malformed a = Malformed a

instance (Arbitrary (Malformed FundsReqBody)) where
    arbitrary = do
        frb <- FundsReqBody <$> arbitrary <*> arbitrary
        case parseFundsReqBody frb of
            Right _ -> arbitrary
            Left _  -> pure $ Malformed frb

instance (Arbitrary (Malformed SubmitTxReqBody)) where
    arbitrary = do
        strb <- SubmitTxReqBody <$> arbitrary <*> arbitrary
        case parseSubmitTxReqBody strb of
            Right _ -> arbitrary
            Left _ -> pure $ Malformed strb