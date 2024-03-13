{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImplicitParams     #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RankNTypes         #-}

module Cardano.Server.Config where

import           Cardano.Api                   (NetworkId (..))
import           Data.Aeson                    (FromJSON (..), ToJSON,
                                                eitherDecodeFileStrict,
                                                genericParseJSON)
import           Data.Aeson.Casing             (aesonPrefix, snakeCase)
import           Data.ByteString               (ByteString)
import           Data.Text                     (Text)
import           GHC.Generics                  (Generic)
import           GHC.Stack                     (HasCallStack)
import           Ledger                        (TxOutRef)
import           PlutusAppsExtra.IO.ChainIndex (ChainIndexProvider)
import           PlutusAppsExtra.IO.Tx         (TxProvider)
import           PlutusAppsExtra.IO.Wallet     (WalletProvider)
import qualified Servant.Client                as Servant

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
    , cBfTokenFilePath        :: Maybe FilePath
    , cMaestroTokenFilePath   :: Maybe FilePath
    , cNetworkId              :: NetworkId
    , cCollateral             :: Maybe TxOutRef
    , cWalletProvider         :: Maybe WalletProvider
    , cChainIndexProvider     :: Maybe ChainIndexProvider
    , cTxProvider             :: Maybe TxProvider
    , cActiveEndpoints        :: [Text]
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