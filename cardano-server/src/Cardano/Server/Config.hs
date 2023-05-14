{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}

module Cardano.Server.Config where

import           Cardano.Api                   (NetworkId (..))
import           Data.Aeson                    (FromJSON (..), eitherDecodeFileStrict, genericParseJSON)
import qualified Data.Aeson                    as J
import           Data.Aeson.Casing             (aesonPrefix, snakeCase)
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
    , cActiveEndpoints        :: [ServerEndpoint]
    } deriving (Show, Generic)

configFile :: FilePath
configFile = "config.json"

loadConfig :: HasCallStack => IO Config
loadConfig = decodeOrErrorFromFile configFile

instance FromJSON Config where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

decodeOrErrorFromFile :: (HasCallStack, FromJSON a) => FilePath -> IO a
decodeOrErrorFromFile = fmap (either error id) . eitherDecodeFileStrict

------------------------------------------------------------------- Endpoints -------------------------------------------------------------------

data ServerEndpoint
    = PingE
    | FundsE
    | NewTxE
    | SubmitTxE
    | ServerTxE
    | StatusE
    deriving Eq

instance Read ServerEndpoint where
    readsPrec _ = \case
        "ping"     -> [(PingE    , "")]
        "funds"    -> [(FundsE   , "")]
        "newTx"    -> [(NewTxE   , "")]
        "submitTx" -> [(SubmitTxE, "")]
        "serverTx" -> [(ServerTxE, "")]
        "status"   -> [(StatusE  , "")]
        _          -> []

instance Show ServerEndpoint where
    show = \case
        PingE     -> "ping"
        FundsE    -> "funds"
        NewTxE    -> "newTx"
        SubmitTxE -> "submitTx"
        ServerTxE -> "serverTx"
        StatusE   -> "status"

instance FromJSON ServerEndpoint where
    parseJSON = \case
        J.String "ping"     -> pure PingE
        J.String "funds"    -> pure FundsE
        J.String "newTx"    -> pure NewTxE
        J.String "submitTx" -> pure SubmitTxE
        J.String "serverTx" -> pure ServerTxE
        J.String "status"   -> pure StatusE
        _                   -> fail "FromJSON ServerEndpoint"
