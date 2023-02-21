{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Server.Client.Example.Main
    ( runExampleClient
    ) where

import           Cardano.Server.Class         (HasServer (..))
import           Cardano.Server.Client.Class  (HasClient (..))
import           Cardano.Server.Client.Client (startClient)
import           Cardano.Server.Example.Main  (ExampleServer)
import           Control.Applicative          (Alternative (..))
import           Control.Monad                (replicateM)
import           Options.Applicative          (help, long, option, short, metavar)
import           Options.Applicative.Types    (readerAsk)
import           PlutusTx.Builtins.Class      (stringToBuiltinByteString)
import           System.Random                (randomIO, randomRIO)

runExampleClient :: IO ()
runExampleClient = startClient @ExampleServer

instance HasClient ExampleServer where
    type ClientInput ExampleServer = InputOf ExampleServer

    parseClientInput = some $ stringToBuiltinByteString <$> option readerAsk
                            (  long  "mint"
                            <> short 'm'
                            <> help  "Token to mint."
                            <> metavar "TOKEN"
                            )

    genClientInput = do
        inputLength <- randomRIO (1, 15)
        let genBbs = stringToBuiltinByteString <$> (randomRIO (2, 8) >>= (`replicateM` randomIO))
        replicateM inputLength genBbs