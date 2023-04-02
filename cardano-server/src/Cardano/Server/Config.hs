{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

module Cardano.Server.Config where

import           Cardano.Api                   (NetworkId (..))
import           Data.Aeson                    (FromJSON (..), eitherDecodeFileStrict, genericParseJSON)
import           Data.Aeson.Casing             (aesonDrop, aesonPrefix, snakeCase)
import           Data.Text                     (Text)
import           GHC.Generics                  (Generic)
import           Ledger                        (TxOutRef)
import           PlutusAppsExtra.IO.Blockfrost (BfToken)
import           PlutusAppsExtra.IO.ChainIndex (ChainIndex)

data Config = Config
    { cHost              :: Text
    , cPort              :: Int
    , cMinUtxosNumber    :: Int
    , cMaxUtxosNumber    :: Int
    , cAuxiliaryEnvFile  :: FilePath
    , cWalletFile        :: Maybe FilePath
    , cBfToken           :: BfToken
    , cNetworkId         :: NetworkId
    , cCollateral        :: Maybe TxOutRef
    , cNodeFilePath      :: FilePath
    , cChainIndex        :: Maybe ChainIndex
    , cInactiveEndpoints :: InactiveEndpoints
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

loadConfig :: IO Config
loadConfig = decodeOrErrorFromFile configFile

instance FromJSON Config where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance FromJSON InactiveEndpoints where
   parseJSON = genericParseJSON $ aesonDrop 10 snakeCase

decodeOrErrorFromFile :: FromJSON a => FilePath -> IO a
decodeOrErrorFromFile = fmap (either error id) . eitherDecodeFileStrict