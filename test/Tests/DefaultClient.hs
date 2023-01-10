{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Tests.DefaultClient where

import           Client.Class           (HasClient(..))
import           Client.Default         (defaultClient)
import           Control.Exception      (handle, SomeException)
import           Control.Monad.IO.Class (MonadIO(..))
import           Data.Aeson             (encode, FromJSON)
import qualified Data.ByteString.Lazy   as LBS
import           Server.Class           (HasServer(InputOf))
import           Server.Internal        (runAppM)
import           System.Directory       (removeFile)

testDefaultClient :: forall s. (HasClient s, FromJSON (InputOf s)) => IO ()
testDefaultClient = runAppM @s $ do
    input <- encode <$> genServerInput @s
    let fp = "testnet/DefaultClientTestInput.json"
    liftIO $ do
        LBS.writeFile fp input
        handle (\(e :: SomeException) -> print e) $ defaultClient @s fp
        removeFile fp