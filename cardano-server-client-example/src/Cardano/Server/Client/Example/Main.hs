{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Server.Client.Example.Main (runTestingClient) where

import Cardano.Server.Client.Class           (HasClient(..))
import Cardano.Server.Client.Client          (startClient)
import Cardano.Server.Example.Main           (TestingServer)
import Control.Monad                         (replicateM)
import Options.Applicative                   (argument, metavar, str, some)
import PlutusTx.Builtins.Class               (stringToBuiltinByteString)
import System.Random                         (randomRIO, randomIO)

runTestingClient :: IO ()
runTestingClient = startClient @TestingServer

instance HasClient TestingServer where

    parseServerInput = some $ stringToBuiltinByteString <$> argument str (metavar "token name")

    genServerInput = do
        inputLength <- randomRIO (1, 15)
        let genBbs = stringToBuiltinByteString <$> (randomRIO (2, 8) >>= (`replicateM` randomIO))
        replicateM inputLength genBbs