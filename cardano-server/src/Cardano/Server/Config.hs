{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

module Cardano.Server.Config where

import           Cardano.Api                     (NetworkId (..))
import           Cardano.Node.Emulator           ()
import           Cardano.Server.Utils.ChainIndex (ChainIndex)
import           Data.Aeson                      (FromJSON (..), eitherDecodeFileStrict, genericParseJSON)
import           Data.Aeson.Casing               (aesonDrop, aesonPrefix, snakeCase)
import           Data.Text                       (Text)
import           GHC.Generics                    (Generic)
import           Ledger                          (TxOutRef)

data Config = Config
    { cServerAddress     :: Text
    , cMinUtxosAmount    :: Int
    , cAuxiliaryEnvFile  :: FilePath
    , cWalletFile        :: FilePath
    , cNetworkId         :: NetworkId
    , cCollateral        :: Maybe TxOutRef
    , cNodeFilePath      :: FilePath
    , cChainIndex        :: ChainIndex
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
decodeOrErrorFromFile = fmap (either error id) . eitherDecodeFileStrict