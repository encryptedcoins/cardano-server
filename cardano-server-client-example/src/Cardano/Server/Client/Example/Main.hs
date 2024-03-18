{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Cardano.Server.Client.Example.Main where

import           Cardano.Server.Client.Client     (autoClientWith, autoPing, autoVersion, manualClient, manualPing, manualVersion,
                                                   pingClient, sendRequest, versionClient)
import           Cardano.Server.Client.Opts       (Interval, autoModeP, manualModeP, mapMode, pingCommand, randomCommand, runWithOpts,
                                                   versionCommand)
import           Cardano.Server.Config            (Creds, decodeOrErrorFromFile)
import           Cardano.Server.Endpoints.Ping    (PingApi)
import           Cardano.Server.Endpoints.Version (VersionApi)
import           Cardano.Server.Error.Servant     (EndpointEnvelope)
import           Cardano.Server.Example.Main      (ExampleApi, ExampleApiEnv (..), MintApi, StatusApi, loadExampleApiEnv)
import           Cardano.Server.Input             (InputContext (..))
import           Cardano.Server.Internal          (AppT (..), loadEnv, runAppT)
import           Cardano.Server.Utils.Wait        (waitTime)
import           Control.Monad                    (forever, replicateM, void)
import           Control.Monad.IO.Class           (MonadIO (liftIO))
import           Data.FileEmbed                   (embedFileIfExists)
import           Ledger                           (TxOutRef (..))
import           Options.Applicative              (Alternative (many, (<|>)), Parser, argument, auto, command, hsubparser, info, progDesc,
                                                   strArgument)
import           PlutusAppsExtra.IO.Wallet        (getWalletAddr, getWalletUtxos)
import           PlutusTx.Builtins                (BuiltinByteString)
import           PlutusTx.Builtins.Class          (stringToBuiltinByteString)
import           Servant                          (Proxy (Proxy))
import           Servant.Client                   (ClientM, client)
import           System.Random                    (randomIO, randomRIO)
import           Control.Monad.Catch              (MonadCatch (..))

runExampleClient :: FilePath -> IO ()
runExampleClient configFp = do
    opts <- runWithOpts commandParser
    let ?creds = embedCreds
    config <- decodeOrErrorFromFile configFp
    auxConfig <- decodeOrErrorFromFile configFp
    auxEnv <- loadExampleApiEnv auxConfig
    env <- loadEnv config auxEnv
    runAppT env $ runCommand opts

-- Embed https cert and key files on compilation
embedCreds :: Creds
embedCreds =
    let keyCred  = $(embedFileIfExists "../key.pem" )
        certCred = $(embedFileIfExists "../certificate.pem")
    in (,) <$> certCred <*> keyCred

data ClientCommand
    = AutoPing       Interval
    | AutoVersion    Interval
    | AutoMint       Interval
    | AutoStatus     Interval
    | ManualPing
    | ManualVersion
    | ManualMint     [(BuiltinByteString, Integer)]
    | ManualStatus   Bool
    | RandomEndpoint Interval
    deriving stock (Show, Eq)

runCommand :: ClientCommand -> AppT ExampleApi IO ()
runCommand = \case
    ManualPing       -> void manualPing
    ManualVersion    -> void manualVersion
    ManualMint   bbs -> void $ genMintContext >>= manualClient @MintApi . mintClient  . (bbs,)
    ManualStatus   b -> void $ manualClient @StatusApi $ statusClient b
    AutoPing       i -> autoPing i
    AutoVersion    i -> autoVersion i
    AutoMint       i -> autoClientWith @MintApi genMintArg i mintClient
    AutoStatus     i -> autoClientWith @StatusApi randomIO i statusClient
    RandomEndpoint i -> forever $ do
        liftIO (randomRIO @Int (0, 3)) >>= \case
            0 -> void $ sendRequest @PingApi pingClient
            1 -> void $ sendRequest @VersionApi versionClient
            2 -> void $ sendRequest @MintApi . mintClient =<< genMintArg
            _ -> void $ sendRequest @StatusApi . statusClient =<< randomIO
        waitTime =<< randomRIO (1, i * 2)

mintClient :: ([(BuiltinByteString, Integer)], InputContext) -> ClientM (EndpointEnvelope MintApi)
mintClient = client (Proxy @MintApi)

statusClient :: Bool -> ClientM (EndpointEnvelope StatusApi)
statusClient = client (Proxy @StatusApi)

commandParser :: Parser ClientCommand
commandParser = hsubparser $ mconcat
    [ pingCommand ManualPing AutoPing
    , versionCommand ManualVersion AutoVersion
    , randomCommand  RandomEndpoint
    , command "mint"    $ info mintP    $ progDesc "Client for mint endpoint."
    , command "status"  $ info statusP  $ progDesc "Client for status endpoint."
    ]

mintP :: Parser ClientCommand
mintP = fmap (mapMode AutoMint ManualMint) $ manualModeP (many $ (,) <$> bbsP <*> amtP) <|> autoModeP
  where
    bbsP = strArgument $ mconcat []
    amtP = argument auto $ mconcat []

statusP :: Parser ClientCommand
statusP = fmap (mapMode AutoStatus ManualStatus) $ manualModeP statusBodyP <|> autoModeP
  where
    statusBodyP = argument auto $ mconcat []

genMintArg :: (MonadIO m, MonadCatch m) => AppT ExampleApi m ([(BuiltinByteString, Integer)], InputContext)
genMintArg = (,) <$> genMintInput <*> genMintContext

genMintInput :: MonadIO m => AppT ExampleApi m [(BuiltinByteString, Integer)]
genMintInput = do
    inputLength <- randomRIO (1, 15)
    let genBbs = stringToBuiltinByteString <$> (randomRIO (2, 8) >>= (`replicateM` randomIO))
    replicateM inputLength $ (,) <$> genBbs <*> randomRIO (1000, 100000)

genMintContext :: (MonadIO m, MonadCatch m) => AppT ExampleApi m InputContext
genMintContext = do
    addr <- getWalletAddr
    utxos <- getWalletUtxos mempty
    pure $ InputContextClient utxos utxos (TxOutRef "" 0) addr