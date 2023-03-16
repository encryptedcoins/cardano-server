{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Server.Client.Example.Main
    ( runExampleClient
    ) where

import           Cardano.Server.Client.Client   (autoWith, manualWith, runClient)
import qualified Cardano.Server.Client.Client   as Client
import           Cardano.Server.Client.Internal (ClientHandle (autoNewTx, autoServerTx, manualNewTx, manualServerTx))
import           Cardano.Server.Example.Main    (ExampleApi, exampleHandle)
import           Cardano.Server.Input           (InputContext)
import           Cardano.Server.Internal        (ServerM)
import           Control.Monad                  (replicateM)
import           Data.Default                   (Default (def))
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           PlutusTx.Builtins              (BuiltinByteString)
import           PlutusTx.Builtins.Class        (stringToBuiltinByteString)
import           System.Random                  (randomIO, randomRIO)

runExampleClient :: IO ()
runExampleClient = runClient exampleHandle $ Client.defaultHandle
    { autoNewTx      = autoWith   genInput
    , autoServerTx   = autoWith   genInput
    , manualNewTx    = manualWith readInput
    , manualServerTx = manualWith readInput
    }

genInput :: ServerM ExampleApi ([BuiltinByteString], InputContext)
genInput = (,def) <$> do
    inputLength <- randomRIO (1, 15)
    let genBbs = stringToBuiltinByteString <$> (randomRIO (2, 8) >>= (`replicateM` randomIO))
    replicateM inputLength genBbs

readInput :: Text -> ServerM ExampleApi ([BuiltinByteString], InputContext)
readInput = pure . (,def) . map stringToBuiltinByteString . words . T.unpack