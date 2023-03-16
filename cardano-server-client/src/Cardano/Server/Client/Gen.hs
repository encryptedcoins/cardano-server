{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Server.Client.Gen where

import           Cardano.Node.Emulator.Generators   (genSomeCardanoApiTx)
import           Cardano.Server.Client.Internal     (ClientEndpoint (EndpointArg), ServerEndpoint (FundsE, SubmitTxE))
import           Cardano.Server.Endpoints.Funds     (FundsReqBody (FundsReqBody))
import           Cardano.Server.Endpoints.Tx.Submit (SubmitTxReqBody (..))
import           Control.Monad.IO.Class             (MonadIO (..))
import           Data.Bifunctor                     (Bifunctor (bimap))
import qualified Data.Text                          as T
import           Hedgehog.Gen                       (sample)
import           Ledger                             (Address, CardanoTx (CardanoApiTx), CurrencySymbol, PubKey, Signature)
import           Plutus.PAB.Arbitrary               ()
import           Test.QuickCheck                    (Arbitrary (..), generate)

randomFundsReqBody :: MonadIO m => m (EndpointArg 'FundsE api)
randomFundsReqBody = liftIO $ generate $ FundsReqBody
    <$> (T.pack . show <$> (arbitrary @Address))
    <*> (T.pack . show <$> (arbitrary @CurrencySymbol))

randomSubmitTxBody :: MonadIO m => m (EndpointArg 'SubmitTxE api)
randomSubmitTxBody = liftIO $ SubmitTxReqBody
    <$> (T.pack . show <$> randomCardanoTx)
    <*> (fmap (bimap (T.pack . show) (T.pack . show)) <$> liftIO (generate $ arbitrary @[(PubKey, Signature)]))

randomCardanoTx :: IO CardanoTx
randomCardanoTx = sample $ CardanoApiTx <$> genSomeCardanoApiTx