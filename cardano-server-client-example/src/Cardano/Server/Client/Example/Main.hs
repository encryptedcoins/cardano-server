{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans  #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}

module Cardano.Server.Client.Example.Main
    ( runExampleClient
    ) where

-- import           Cardano.Server.Class         (HasServer (..))
-- import           Cardano.Server.Client.Class  (HasClient (..))
-- import           Cardano.Server.Client.Client (startClient)
import           Cardano.Server.Example.Main  (ExampleApi, exampleHandle)
import Cardano.Server.Client.Client -- (runClient, autoWith)
import Cardano.Server.Internal
import Cardano.Server.Client.Internal
import PlutusTx.Builtins (BuiltinByteString)
import qualified Cardano.Server.Client.Client as Client
import Cardano.Server.Input (InputContext)
import Data.Default
import System.Random
import PlutusTx.Builtins.Class (stringToBuiltinByteString)
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T

runExampleClient :: IO ()
runExampleClient = runClient @ExampleApi exampleHandle $ Client.defaultHandle
    { autoNewTx      = autoWith   @'NewTxE    genInput
    , autoServerTx   = autoWith   @'ServerTxE genInput
    , manualNewTx    = manualWith @'NewTxE    readInput
    , manualServerTx = manualWith @'ServerTxE readInput
    }

genInput :: ServerM ExampleApi ([BuiltinByteString], InputContext)
genInput = (,def) <$> do
    inputLength <- randomRIO (1, 15)
    let genBbs = stringToBuiltinByteString <$> (randomRIO (2, 8) >>= (`replicateM` randomIO))
    replicateM inputLength genBbs

readInput :: Text -> ServerM ExampleApi ([BuiltinByteString], InputContext)
readInput = pure . (,def) . map stringToBuiltinByteString . words . T.unpack