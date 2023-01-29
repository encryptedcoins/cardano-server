{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Server.Client.Example.Main (runExampleClient) where

import Cardano.Server.Client.Class           (HasClient(..))
import Cardano.Server.Client.Client          (startClient)
import Cardano.Server.Example.Main           (ExampleServer)
import Control.Monad                         (replicateM)
import Options.Applicative                   (argument, metavar, str, some)
import PlutusTx.Builtins.Class               (stringToBuiltinByteString)
import System.Random                         (randomRIO, randomIO)

runExampleClient :: IO ()
runExampleClient = startClient @ExampleServer

instance HasClient ExampleServer where

    parseServerInput = some $ stringToBuiltinByteString <$> argument str (metavar "token name")

    genServerInput = do
        inputLength <- randomRIO (1, 15)
        let genBbs = stringToBuiltinByteString <$> (randomRIO (2, 8) >>= (`replicateM` randomIO))
        replicateM inputLength genBbs