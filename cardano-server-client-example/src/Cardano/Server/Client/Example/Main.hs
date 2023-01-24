{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Server.Client.Example.Main (runTestingClient) where

import Cardano.Server.Class                  (HasServer(..))
import Cardano.Server.Client.Class           (HasClient(..))
import Cardano.Server.Client.Client          (startClient)
import Cardano.Server.Example.Main           (ExampleServer)
import Control.Monad                         (replicateM)
import Options.Applicative                   (argument, metavar, str, some)
import PlutusTx.Builtins.Class               (stringToBuiltinByteString)
import System.Random                         (randomRIO, randomIO)

runTestingClient :: IO ()
runTestingClient = startClient @ExampleServer

instance HasClient ExampleServer where
    type ClientInput ExampleServer = InputOf ExampleServer

    parseClientInput = some $ stringToBuiltinByteString <$> argument str (metavar "token name")

    genClientInput = do
        inputLength <- randomRIO (1, 15)
        let genBbs = stringToBuiltinByteString <$> (randomRIO (2, 8) >>= (`replicateM` randomIO))
        replicateM inputLength genBbs
    
    toServerInput = pure