{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImplicitParams     #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <$>" #-}

module Cardano.Server.Config where

import           Cardano.Api                        (NetworkId (..), prettyPrintJSON, writeFileJSON)
import           Cardano.Mnemonic                   (MkSomeMnemonic (..))
import           Control.Applicative                (Applicative (..))
import           Control.Exception                  (SomeException)
import           Control.Lens                       ((&), (.~))
import           Control.Monad                      (MonadPlus (mzero), join, when, (<=<), (>=>))
import           Control.Monad.Catch                (try)
import           Control.Monad.Extra                (unlessM, whenM)
import           Data.Aeson                         (FromJSON (..), ToJSON (..), eitherDecodeFileStrict, genericParseJSON, (.=))
import qualified Data.Aeson                         as J
import           Data.Aeson.Casing                  (aesonPrefix, snakeCase)
import           Data.Aeson.Lens                    (key)
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString                    as BS
import           Data.Either.Extra                  (eitherToMaybe)
import           Data.List.Extra                    (breakOnEnd, notNull)
import           Data.List.NonEmpty                 (NonEmpty ((:|)), nonEmpty)
import           Data.Maybe                         (fromMaybe, isJust, isNothing)
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import qualified Data.Text.IO                       as T
import           GHC.Generics                       (Generic)
import           GHC.Stack                          (HasCallStack)
import           Ledger                             (TxOutRef)
import           PlutusAppsExtra.IO.ChainIndex      (ChainIndexProvider)
import qualified PlutusAppsExtra.IO.ChainIndex      as CI
import           PlutusAppsExtra.IO.Tx              (TxProvider)
import qualified PlutusAppsExtra.IO.Tx              as Tx
import           PlutusAppsExtra.IO.Wallet          (RestoredWallet (..), WalletProvider, restoreWalletFromFile)
import qualified PlutusAppsExtra.IO.Wallet          as Wallet
import           PlutusAppsExtra.IO.Wallet.Internal (addressFromMnemonic)
import           PlutusAppsExtra.Utils.Address      (bech32ToAddress, addressToBech32)
import qualified Servant.Client                     as Servant
import           System.Directory                   (createDirectoryIfMissing, doesFileExist)
import           Text.Read                          (readMaybe)

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
    , cWalletFile             :: FilePath
    , cBfTokenFilePath        :: Maybe FilePath
    , cMaestroTokenFilePath   :: Maybe FilePath
    , cNetworkId              :: NetworkId
    , cCollateral             :: Maybe TxOutRef
    , cDataProviders          :: ConfigDataProviders
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

data ConfigDataProviders
    = Lightweight FilePath
    -- ^ File with addresses tracked
    | Cardano FilePath
    -- ^ Node file path
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

dataProvidersFromConfigDataProviders :: ConfigDataProviders -> IO (WalletProvider, ChainIndexProvider, TxProvider)
dataProvidersFromConfigDataProviders = \case
    Lightweight fp -> do
        addrs <- fromMaybe (error "Unparsable addresses for lightweight wallet provider.") <$> readAddrs fp
        pure (Wallet.Lightweight addrs, CI.Maestro, Tx.Maestro)
    Cardano fp -> pure (Wallet.Cardano, CI.Kupo, Tx.Cardano fp)
  where
    readAddrs = fmap (>>= (mapM bech32ToAddress >=> nonEmpty)) . J.decodeFileStrict @[Text]

------------------------------------------------------------------- Inintialisation -------------------------------------------------------------------

-- | Ask user to type all missing information
initialiseConfig :: FilePath -> IO ()
initialiseConfig configFp = do
    Config{..} <- decodeOrErrorFromFile configFp
    sequence_ $ initialiseToken "blockfrost" <$> cBfTokenFilePath
    sequence_ $ initialiseToken "maestro" <$> cMaestroTokenFilePath
    initialiseNode configFp Config{..}
    initialiseWallet cWalletFile
    initialiseWalletProvider Config{..}

initialiseNode :: FilePath -> Config -> IO ()
initialiseNode _        Config{cDataProviders = Lightweight _} = pure ()
initialiseNode configFp Config{cDataProviders = Cardano nodeFp} = unlessM (doesFileExist nodeFp) $ do
    putStrLn "you have node-based providers but cardano node socket doesn't exists in specified path"
    putStrLn "enter correct cardano-node socket file path"
    fp <- getLine
    config_val <- decodeOrErrorFromFile @J.Value configFp
    writeJSONPretty configFp $ config_val & key "cDataProviders" .~ toJSON (Cardano fp)
    decodeOrErrorFromFile configFp >>= initialiseNode configFp

initialiseWallet :: FilePath -> IO ()
initialiseWallet cWalletFile =
    unlessM (liftA2 (&&) (doesFileExist cWalletFile) (isJust <$> readWalletFile)) $ do
        putStrLn "wallet doesn't exists or wallet file is corrupted"
        putStrLn "enter your wallet name"
        name <- T.getLine
        mnemonic_txt <- enterMnemonic
        putStrLn "enter your wallet passphrase"
        passphrase <- getLine
        addMissingDirectories cWalletFile
        res <- writeFileJSON cWalletFile $ J.object
            [ "name" .= name
            , "mnemonic_sentence" .= T.words mnemonic_txt
            , "passphrase" .= passphrase
            ]
        either print pure res
  where
    readWalletFile = fmap (join . eitherToMaybe) $ try @_ @SomeException $ J.decodeFileStrict @RestoredWallet cWalletFile
    enterMnemonic = do
        putStrLn "enter your wallet mnemonic (24 words)"
        mnemonic_txt <- T.getLine
        case  mkSomeMnemonic @'[ 24 ] $ T.words mnemonic_txt of
            Right _ -> pure mnemonic_txt
            Left err -> print err >> enterMnemonic

initialiseWalletProvider :: Config -> IO ()
initialiseWalletProvider Config{cDataProviders = Cardano _} = pure ()
initialiseWalletProvider Config{cDataProviders = Lightweight fp, ..} =
    whenM (isNothing <$> readAddrs) $ do
        putStrLn "no addresses specified for lightweight wallet provider or addresses file is corrupted"
        putStrLn "now they will be selected automatically, but you can manually change them at any time"
        putStrLn "keep in mind that each additional address also incurs additional costs in the form of provider credits"
        mnemonic <- mnemonicSentence <$> restoreWalletFromFile cWalletFile
        addrs <- (:| []) <$> addressFromMnemonic cNetworkId mnemonic
        addMissingDirectories fp
        writeFileJSON fp (addressToBech32  cNetworkId <$> addrs) >>= either print pure
  where
    readAddrs =  fmap ((mapM bech32ToAddress >=> nonEmpty) <=< (join . eitherToMaybe))
        $ try @_ @SomeException $ J.decodeFileStrict @[Text] fp

initialiseToken :: String -> FilePath -> IO String
initialiseToken tokenName fp = do
    unlessM (doesFileExist fp) $ do
        putStrLn $ tokenName <> " token file doesn't exists."
        putStrLn $ "enter your " <> tokenName <> " token."
        token <- getLine
        writeFile fp $ withQuotes token
    readFile fp
  where
    withQuotes x@('"' : _) = x
    withQuotes x = '"' : x <> "\""

writeJSONPretty :: FilePath -> J.Value -> IO ()
writeJSONPretty fp val = BS.writeFile fp $ prettyPrintJSON val

addMissingDirectories :: FilePath -> IO ()
addMissingDirectories fp = let (dir,_) = breakOnEnd "/" fp in when (notNull dir) $ createDirectoryIfMissing True dir

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
    parseJSON = J.withText "ServerEndpoint" $ maybe mzero pure . readMaybe . T.unpack

instance ToJSON ServerEndpoint where
    toJSON = J.String . T.pack . show
