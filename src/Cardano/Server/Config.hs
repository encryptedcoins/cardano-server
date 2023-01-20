{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

module Cardano.Server.Config where

import           Cardano.Api          (NetworkId(..))
import           Data.Aeson           (FromJSON(..), eitherDecode, genericParseJSON)
import           Data.Aeson.Casing    (aesonPrefix, snakeCase)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Text            (Text)
import           GHC.Generics         (Generic)
import           Ledger.Params        ()

data Config = Config
    { cServerAddress     :: Text
    , cNodeAddress       :: Text
    , cChainIndexAddress :: Text
    , cMinUtxosAmount    :: Int
    , cAuxiliaryEnvFile  :: FilePath
    , cWalletFile        :: FilePath
    , cNetworkId         :: NetworkId
    } deriving (Show, Generic)

configFile :: FilePath
configFile = "testnet/config.json"

loadConfig :: IO Config
loadConfig = decodeOrErrorFromFile configFile

instance FromJSON Config where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

decodeOrErrorFromFile :: FromJSON a => FilePath -> IO a
decodeOrErrorFromFile = fmap (either error id . eitherDecode . LBS.fromStrict) . BS.readFile