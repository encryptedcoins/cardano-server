{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Server.Client.Example.Main where

import           Cardano.Server.Client.Client (runClient)
import           Cardano.Server.Client.Handle (ClientHandle (..), autoWith, autoWithRandom, manualWith, manualWithRead)
import           Cardano.Server.Example.Main  (ExampleApi, exampleServerHandle)
import           Cardano.Server.Input         (InputContext)
import           Control.Monad                (replicateM)
import           Control.Monad.IO.Class       (MonadIO (liftIO))
import           Data.Default                 (Default (def))
import           Data.List                    (nub)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           PlutusTx.Builtins            (BuiltinByteString)
import           PlutusTx.Builtins.Class      (stringToBuiltinByteString)
import           System.Random                (randomIO, randomRIO)

runExampleClient :: IO ()
runExampleClient = runClient exampleServerHandle exampleClientHandle

exampleClientHandle :: ClientHandle ExampleApi
exampleClientHandle = def
    { autoNewTx      = autoWith   genInput
    , autoServerTx   = autoWith   genInput
    , autoStatus     = autoWithRandom   
    , manualNewTx    = manualWith readInput
    , manualServerTx = manualWith readInput
    , manualStatus   = manualWithRead
    }

genInput :: MonadIO m => m ([BuiltinByteString], InputContext)
genInput = fmap ((,def) . nub) $ liftIO $ do
    inputLength <- randomRIO (1, 15)
    let genBbs = stringToBuiltinByteString <$> (randomRIO (2, 8) >>= (`replicateM` randomIO))
    replicateM inputLength genBbs

readInput :: Monad m => Text -> m ([BuiltinByteString], InputContext)
readInput = pure . (,def) . map (stringToBuiltinByteString . T.unpack) . T.splitOn ","