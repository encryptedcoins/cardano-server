{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module DefaultClient where

import           Cardano.Server.Class          (HasServer(InputOf))
import           Cardano.Server.Client.Class   (HasClient(..))
import           Cardano.Server.Client.Default (defaultClient)
import           Cardano.Server.Internal       (runAppM)
import           Control.Exception             (handle, SomeException)
import           Control.Monad.IO.Class        (MonadIO(..))
import           Data.Aeson                    (encode, FromJSON)
import qualified Data.ByteString.Lazy          as LBS
import           System.Directory              (removeFile)

testDefaultClient :: forall s. (HasClient s, FromJSON (InputOf s)) => IO ()
testDefaultClient = runAppM @s $ do
    input <- encode <$> genClientInput @s
    let fp = "testnet/DefaultClientTestInput.json"
    liftIO $ do
        LBS.writeFile fp input
        handle (\(e :: SomeException) -> print e) $ defaultClient @s fp
        removeFile fp