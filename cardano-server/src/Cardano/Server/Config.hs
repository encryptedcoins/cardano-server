{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

module Cardano.Server.Config where

import           Cardano.Api           (NetworkId(..))
import           Data.Aeson            (FromJSON(..), eitherDecode, genericParseJSON)
import           Data.Aeson.Casing     (aesonPrefix, snakeCase, aesonDrop)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as LBS
import           Data.Text             (Text)
import           GHC.Generics          (Generic)
import           Cardano.Node.Emulator ()

data Config = Config
    { cServerAddress     :: Text
    , cNodeAddress       :: Text
    , cChainIndexAddress :: Text
    , cMinUtxosAmount    :: Int
    , cAuxiliaryEnvFile  :: FilePath
    , cWalletFile        :: FilePath
    , cNetworkId         :: NetworkId
    , cInactiveEndpoints :: InactiveEndpoints
    } deriving (Show, Generic)

data InactiveEndpoints = InactiveEndpoints
    { isInactivePing         :: Bool
    , isInactiveFunds        :: Bool
    , isInactiveAddSignature :: Bool
    , isInactiveSubmitTx     :: Bool
    , isInactiveServerTx     :: Bool
    , isInactiveNewTx        :: Bool
    } deriving (Show, Generic)

configFile :: FilePath
configFile = "testnet/config.json"

loadConfig :: IO Config
loadConfig = decodeOrErrorFromFile configFile

instance FromJSON Config where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance FromJSON InactiveEndpoints where
   parseJSON = genericParseJSON $ aesonDrop 10 snakeCase

decodeOrErrorFromFile :: FromJSON a => FilePath -> IO a
decodeOrErrorFromFile = fmap (either error id . eitherDecode . LBS.fromStrict) . BS.readFile