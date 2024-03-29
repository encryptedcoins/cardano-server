{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ImplicitParams       #-}
{-# LANGUAGE NumericUnderscores   #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Server.Client.Example.Main where

import           Cardano.Server.Client.Client (runClient)
import           Cardano.Server.Client.Handle (ClientHandle (..), autoWith, autoWithRandom, manualWith, manualWithRead)
import           Cardano.Server.Config        (decodeOrErrorFromFile)
import           Cardano.Server.Example.Main  (ExampleApi, exampleServerHandle)
import           Cardano.Server.Input         (InputContext)
import           Cardano.Server.Main          (embedCreds)
import           Control.Monad                (replicateM)
import           Control.Monad.IO.Class       (MonadIO (liftIO))
import           Data.Bifunctor               (Bifunctor (..))
import           Data.Default                 (Default (def))
import           Data.List.Extra              (breakOn, nub)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
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
    { autoNewTx      = autoWith genInput
    , autoServerTx   = autoWith genInput
    , autoStatus     = autoWithRandom
    , manualNewTx    = manualWith readInput
    , manualServerTx = manualWith readInput
    , manualStatus   = manualWithRead
    }

genInput :: MonadIO m => m ([(BuiltinByteString, Integer)], InputContext)
genInput = fmap ((,def) . nub) $ liftIO $ do
    inputLength <- randomRIO (1, 15)
    let genBbs = stringToBuiltinByteString <$> (randomRIO (2, 8) >>= (`replicateM` randomIO))
    replicateM inputLength $ (,) <$> genBbs <*> randomRIO (1, 10_000_000)

readInput :: Monad m => Text -> m ([(BuiltinByteString, Integer)], InputContext)
readInput = pure . (,def) . map (bimap stringToBuiltinByteString (read . tail) . breakOn "=" . T.unpack) . T.splitOn ","