{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ImplicitParams       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Server.Example.Main where

import           Cardano.Node.Emulator.Params       (Params (Params), pParamsFromProtocolParams)
import           Cardano.Server.Config              (decodeOrErrorFromFile, AuxillaryConfigOf, Config (..), withDefault)
import           Cardano.Server.Endpoints.Ping      (PingApi, pingHandler)
import           Cardano.Server.Endpoints.Version   (VersionApi, versionEndpointHandler)
import           Cardano.Server.Error               (IsCardanoServerError (..))
import           Cardano.Server.Error.Servant       (Throws)
import           Cardano.Server.Example.Input       (TestPolicyInput (TestPolicyInput))
import           Cardano.Server.Example.OffChain    (testMintTx)
import           Cardano.Server.Handler             (wrapHandler)
import           Cardano.Server.Input               (InputContext (..))
import           Cardano.Server.Internal            (AuxillaryEnvOf, ServerM, loadEnv, mkServerClientEnv, AppT)
import           Cardano.Server.Main                (embedCreds, runServer)
import           Cardano.Server.Tx                  (mkTx)
import           Cardano.Server.Utils.Logger        (logMsg, (.<))
import           Control.Monad                      (when)
import           Control.Monad.Catch                (Exception, MonadThrow (throwM))
import           Control.Monad.IO.Class             (MonadIO (..))
import           Data.Aeson                         (FromJSON (..), genericParseJSON)
import           Data.Aeson.Casing                  (aesonPrefix, snakeCase)
import           Data.Default                       (Default (..))
import           Data.Functor                       (($>))
import           Data.List                          (nub)
import           Data.Text                          (Text)
import           Development.GitRev                 (gitCommitDate, gitHash)
import           GHC.Generics                       (Generic)
import           Ledger                             (NetworkId, TxOutRef)
import           Paths_cardano_server_example       (version)
import           Plutus.V2.Ledger.Api               (BuiltinByteString)
import           PlutusAppsExtra.Api.Maestro        (MaestroToken)
import           PlutusAppsExtra.IO.ChainIndex      (ChainIndexProvider (..))
import qualified PlutusAppsExtra.IO.ChainIndex      as ChainIndex
import           PlutusAppsExtra.IO.Tx              (TxProvider)
import qualified PlutusAppsExtra.IO.Tx              as Tx
import           PlutusAppsExtra.IO.Wallet          (RestoredWallet, WalletProvider, getWalletAddr)
import qualified PlutusAppsExtra.IO.Wallet          as Wallet
import           Servant                            (HasServer (ServerT), JSON, NoContent (..), Post, Proxy (Proxy), ReqBody,
                                                     type (:<|>) (..), type (:>))
import           Servant.Client                     (runClientM)
import           Servant.Client.Internal.HttpClient (client)

type ExampleApi
    =    PingApi
    :<|> VersionApi
    :<|> MintApi
    :<|> StatusApi

type instance AuxillaryConfigOf ExampleApi = ExampleApiConfig

data ExampleApiConfig = ExampleApiConfig
    { cNetworkId              :: NetworkId
    , cProtocolParametersFile :: FilePath
    , cSlotConfigFile         :: FilePath
    , cNodeFilePath           :: FilePath
    , cWalletFile             :: Maybe FilePath
    , cMaestroTokenFilePath   :: Maybe FilePath
    , cWalletProvider         :: Maybe WalletProvider
    , cChainIndexProvider     :: Maybe ChainIndexProvider
    , cTxProvider             :: Maybe TxProvider
    , cCollateral             :: Maybe TxOutRef
    } deriving (Show, Eq, Generic)

instance FromJSON ExampleApiConfig where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

type instance AuxillaryEnvOf ExampleApi = ExampleApiEnv

data ExampleApiEnv = ExampleApiEnv
    { envNetworkId          :: NetworkId
    , envProtocolParams     :: Params
    , envWallet             :: Maybe RestoredWallet
    , envWalletProvider     :: WalletProvider
    , envChainIndexProvider :: ChainIndexProvider
    , envTxProvider         :: TxProvider
    , envCollateral         :: Maybe TxOutRef
    , envMaestroToken       :: Maybe MaestroToken
    } deriving (Show, Eq)

loadExampleApiEnv :: MonadIO m => ExampleApiConfig -> m ExampleApiEnv
loadExampleApiEnv ExampleApiConfig{..} = liftIO $ do
    slotConfig            <- decodeOrErrorFromFile cSlotConfigFile
    pp                    <- decodeOrErrorFromFile cProtocolParametersFile
    envWallet             <- sequence $ decodeOrErrorFromFile <$> cWalletFile
    envMaestroToken       <- sequence $ decodeOrErrorFromFile <$> cMaestroTokenFilePath
    envWalletProvider     <- withDefault Wallet.Cardano cWalletProvider
    envChainIndexProvider <- withDefault ChainIndex.Kupo cChainIndexProvider
    envTxProvider         <- withDefault Tx.Cardano cTxProvider
    pure $ ExampleApiEnv
        { envNetworkId          = cNetworkId
        , envCollateral         = cCollateral
        , envProtocolParams     = Params slotConfig (pParamsFromProtocolParams pp) cNetworkId
        , ..
        }

exampleServer :: ServerT ExampleApi (ServerM ExampleApi)
exampleServer = pingHandler
    :<|> versionEndpointHandler version $(gitHash) $(gitCommitDate)
    :<|> mintHandler
    :<|> statusEndpointHandler

runExampleServer :: FilePath -> IO ()
runExampleServer configFp = do
    config <- decodeOrErrorFromFile configFp
    let ?creds = embedCreds
    auxEnv <- loadExampleApiEnv $ cAuxilaryConfig config
    env <- loadEnv config auxEnv
    runServer @ExampleApi exampleServer env def

--------------------------------------------
-- | * Mint
--------------------------------------------

type MintApi = "mint"
    :> Throws ExampleApiError
    :> ReqBody '[JSON] ([(BuiltinByteString, Integer)], InputContext)
    :> Post '[JSON] NoContent

data ExampleApiError
    = HasDuplicates
    deriving (Show, Exception)

instance IsCardanoServerError ExampleApiError where
    errStatus _ = toEnum 422
    errMsg _ = "The request contains duplicate tokens and will not be processed."

mintHandler :: ([(BuiltinByteString, Integer)], InputContext) -> ServerM ExampleApi NoContent
mintHandler arg@(bbsWithAmt, ctx) = wrapHandler @MintApi $ do
    logMsg $ "Recieved mint request:\n" .< arg
    let hasDuplicates = length (snd <$> bbsWithAmt) /= length (nub $ snd <$> bbsWithAmt)
    when hasDuplicates $ throwM HasDuplicates
    addr <- getWalletAddr
    mkTx [addr] ctx [testMintTx $ uncurry TestPolicyInput $ unzip bbsWithAmt] $> NoContent

--------------------------------------------
-- | * Status
--------------------------------------------

type StatusApi = "status"
    :> Throws ExampleStatusEndpointError
    :> ReqBody '[JSON] Bool
    :> Post '[JSON] Text

data ExampleStatusEndpointError
    = ExampleStatusEndpointError
    deriving (Show, Exception, Generic)

instance IsCardanoServerError ExampleStatusEndpointError where
    errStatus _ = toEnum 422
    errMsg _ = "This is an example of an error in the status endpoint."

statusEndpointHandler :: Bool -> ServerM ExampleApi Text
statusEndpointHandler b = wrapHandler @StatusApi $
    if b
    then pure "This is an example of a status endpoint."
    else throwM ExampleStatusEndpointError

checkStatusEndpoint :: (MonadThrow m, MonadIO m) => AppT ExampleApi m (Either Text ())
checkStatusEndpoint = do
    env <- mkServerClientEnv
    res <- liftIO $ runClientM (client @StatusApi Proxy True) env
    either throwM (const $ pure $ Right ()) res