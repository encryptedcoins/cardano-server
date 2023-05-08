{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

module Cardano.Server.Config where

import           Cardano.Api                   (NetworkId (..))
import           Data.Aeson                    (FromJSON (..), eitherDecodeFileStrict, genericParseJSON)
import           Data.Aeson.Casing             (aesonDrop, aesonPrefix, snakeCase)
import           Data.Text                     (Text)
import           GHC.Generics                  (Generic)
import           GHC.Stack                     (HasCallStack)
import           Ledger                        (TxOutRef)
import           PlutusAppsExtra.IO.Blockfrost (BfToken)
import           PlutusAppsExtra.IO.ChainIndex (ChainIndex)

data Config = Config
    { cHost                   :: Text
    , cPort                   :: Int
    , cMinUtxosNumber         :: Int
    , cMaxUtxosNumber         :: Int
    , cProtocolParametersFile :: FilePath
    , cAuxiliaryEnvFile       :: FilePath
    , cNodeFilePath           :: FilePath
    , cWalletFile             :: Maybe FilePath
    , cBfToken                :: Maybe BfToken
    , cNetworkId              :: NetworkId
    , cCollateral             :: Maybe TxOutRef
    , cChainIndex             :: Maybe ChainIndex
    , cInactiveEndpoints      :: InactiveEndpoints
    } deriving (Show, Generic)

data InactiveEndpoints = InactiveEndpoints
    { isInactivePing         :: Bool
    , isInactiveFunds        :: Bool
    , isInactiveSubmitTx     :: Bool
    , isInactiveServerTx     :: Bool
    , isInactiveNewTx        :: Bool
    , isInactiveStatus       :: Bool
    } deriving (Show, Generic)

configFile :: FilePath
configFile = "config.json"

loadConfig :: HasCallStack => IO Config
loadConfig = decodeOrErrorFromFile configFile

instance FromJSON Config where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance FromJSON InactiveEndpoints where
   parseJSON = genericParseJSON $ aesonDrop 10 snakeCase

decodeOrErrorFromFile :: (HasCallStack, FromJSON a) => FilePath -> IO a
decodeOrErrorFromFile = fmap (either error id) . eitherDecodeFileStrict