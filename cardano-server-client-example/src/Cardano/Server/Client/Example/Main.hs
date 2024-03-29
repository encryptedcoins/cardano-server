{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ImplicitParams       #-}
{-# LANGUAGE NumericUnderscores   #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Server.Client.Example.Main where

import           Cardano.Server.Client.Client (runClient)
import           Cardano.Server.Client.Handle (ClientHandle (..), autoWith, autoWithRandom, manualWith, manualWithRead)
import           Cardano.Server.Config        (decodeOrErrorFromFile)
import           Cardano.Server.Example.Main  (ExampleApi, exampleServerHandle)
import           Cardano.Server.Input         (InputContext (InputContextClient))
import           Cardano.Server.Internal
import           Cardano.Server.Main          (embedCreds)
import           Control.Monad                (replicateM)
import           Control.Monad.IO.Class       (MonadIO (liftIO))
import           Data.Bifunctor               (Bifunctor (..))
import           Data.Default                 (Default (def))
import           Data.List.Extra              (breakOn, nub)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Ledger                       (TxId (TxId), TxOutRef (..))
import           PlutusAppsExtra.IO.Wallet    (HasWalletProvider (getWalletAddr), getWalletUtxos)
import           PlutusTx.Builtins            (BuiltinByteString)
import           PlutusTx.Builtins.Class      (stringToBuiltinByteString)
import           System.Random                (randomIO, randomRIO)

runExampleClient :: FilePath -> IO ()
runExampleClient configFp = do
    config <- decodeOrErrorFromFile configFp
    let ?creds = embedCreds
    runClient config exampleServerHandle exampleClientHandle

exampleClientHandle :: ClientHandle ExampleApi
exampleClientHandle = def
    { autoNewTx      = autoWith $ withCtx genInput
    , autoServerTx   = autoWith $ withCtx genInput
    , autoStatus     = autoWithRandom
    , manualNewTx    = manualWith $ withCtx . readInput
    , manualServerTx = manualWith $ withCtx . readInput
    , manualStatus   = manualWithRead
    }
  where
    withCtx ma = (,) <$> ma <*> mkCtx

mkCtx :: ServerM ExampleApi InputContext
mkCtx = do
    addr <- getWalletAddr
    utxos <- getWalletUtxos mempty
    pure $ InputContextClient mempty utxos (TxOutRef (TxId "") 1) addr

genInput :: MonadIO m => m [(BuiltinByteString, Integer)]
genInput = fmap nub $ liftIO $ do
    inputLength <- randomRIO (1, 15)
    let genBbs = stringToBuiltinByteString <$> (randomRIO (2, 8) >>= (`replicateM` randomIO))
    replicateM inputLength $ (,) <$> genBbs <*> randomRIO (1, 10_000_000)

readInput :: Monad m => Text -> m [(BuiltinByteString, Integer)]
readInput = pure . map (bimap stringToBuiltinByteString (read . tail) . breakOn "=" . T.unpack) . T.splitOn ","