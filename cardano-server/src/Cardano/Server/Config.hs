{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImplicitParams     #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}

module Cardano.Server.Config where

import           Cardano.Api                    (NetworkId (..))
import           Control.Monad
import           Data.Aeson                     (FromJSON (..), ToJSON, eitherDecodeFileStrict, genericParseJSON)
import qualified Data.Aeson                     as J
import           Data.Aeson.Casing              (aesonPrefix, snakeCase)
import           Data.ByteString                (ByteString)
import           Data.Text                      (Text)
import           GHC.Generics                   (Generic)
import           GHC.Stack                      (HasCallStack)
import           Ledger                         (TxOutRef)
import           PlutusAppsExtra.Api.Blockfrost (BfToken)
import           PlutusAppsExtra.IO.ChainIndex  (ChainIndex)
import qualified Servant.Client                 as Servant

data Config = Config
    { cHost                   :: Text
    , cPort                   :: Int
    , cHyperTextProtocol      :: HyperTextProtocol
    , cMinUtxosNumber         :: Int
    , cMaxUtxosNumber         :: Int
    , cDiagnosticsInterval    :: Maybe Int
    , cProtocolParametersFile :: FilePath
    , cSlotConfigFile         :: FilePath
    , cAuxiliaryEnvFile       :: FilePath
    , cNodeFilePath           :: FilePath
    , cWalletFile             :: Maybe FilePath
    , cBfToken                :: Maybe BfToken
    , cNetworkId              :: NetworkId
    , cCollateral             :: Maybe TxOutRef
    , cChainIndex             :: Maybe ChainIndex
    , cActiveEndpoints        :: [ServerEndpoint]
    } deriving (Show, Generic)

instance FromJSON Config where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

decodeOrErrorFromFile :: (HasCallStack, FromJSON a) => FilePath -> IO a
decodeOrErrorFromFile = fmap (either error id) . eitherDecodeFileStrict

data HyperTextProtocol = HTTP | HTTPS
    deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

type Creds = Maybe (ByteString, ByteString)

type HasCreds = ?creds :: Creds

schemeFromProtocol :: HyperTextProtocol -> Servant.Scheme
schemeFromProtocol = \case
    HTTP  -> Servant.Http
    HTTPS -> Servant.Https

------------------------------------------------------------------- Class -------------------------------------------------------------------

class CardanoServerConfig c where
    configHost              :: c -> Text
    configPort              :: c -> Int
    configHyperTextProtocol :: c -> HyperTextProtocol

instance CardanoServerConfig Config where
    configHost = cHost
    configPort = cPort
    configHyperTextProtocol = cHyperTextProtocol

------------------------------------------------------------------- Endpoints -------------------------------------------------------------------

data ServerEndpoint
    = PingE
    | UtxosE
    | NewTxE
    | SubmitTxE
    | ServerTxE
    | StatusE
    | VersionE
    deriving Eq

instance Read ServerEndpoint where
    readsPrec _ = \case
        "ping"     -> [(PingE    , "")]
        "utxos"    -> [(UtxosE   , "")]
        "newTx"    -> [(NewTxE   , "")]
        "submitTx" -> [(SubmitTxE, "")]
        "serverTx" -> [(ServerTxE, "")]
        "status"   -> [(StatusE  , "")]
        "version"  -> [(VersionE  , "")]
        _          -> []

instance Show ServerEndpoint where
    show = \case
        PingE     -> "ping"
        UtxosE    -> "utxos"
        NewTxE    -> "newTx"
        SubmitTxE -> "submitTx"
        ServerTxE -> "serverTx"
        StatusE   -> "status"
        VersionE  -> "version"

instance FromJSON ServerEndpoint where
    parseJSON = J.withText "ServerEndpoint" $ \case
        "ping"     -> pure PingE
        "utxos"    -> pure UtxosE
        "newTx"    -> pure NewTxE
        "submitTx" -> pure SubmitTxE
        "serverTx" -> pure ServerTxE
        "status"   -> pure StatusE
        "version"  -> pure VersionE
        _          -> mzero
