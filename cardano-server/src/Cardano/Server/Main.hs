{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE ImplicitParams            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE QuantifiedConstraints     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

module Cardano.Server.Main where

import           Cardano.Server.Config                (AuxillaryConfigOf, Creds, HasCreds, HyperTextProtocol (..))
import           Cardano.Server.Diagnostics           (doDiagnostics, pingDiagnostics)
import           Cardano.Server.EndpointName          (GetEdpointNames)
import           Cardano.Server.Error.Class           (logCriticalExceptions)
import           Cardano.Server.Internal              (AppT (..), Env (..), ServerM, runAppT)
import           Cardano.Server.Utils.Logger          (logMsg, (.<))
import           Control.Exception                    (throw)
import           Control.Monad.Catch                  (Handler (..), catches)
import           Data.Aeson                           (FromJSON)
import           Data.Default                         (Default (..))
import           Data.FileEmbed                       (embedFileIfExists)
import           Data.Text                            (Text)
import qualified Data.Text.Encoding                   as T
import qualified Data.Text.IO                         as T
import           Network.HTTP.Client                  (path)
import qualified Network.Wai                          as Wai
import qualified Network.Wai.Handler.Warp             as Warp
import qualified Network.Wai.Handler.WarpTLS          as Warp
import           Network.Wai.Middleware.Cors          (CorsResourcePolicy (..), cors, simpleCorsResourcePolicy)
import           PlutusAppsExtra.Api.Kupo             (pattern KupoConnectionError)
import           PlutusAppsExtra.IO.ChainIndex.Plutus (pattern PlutusChainIndexConnectionError)
import           PlutusAppsExtra.IO.Wallet.Cardano    (pattern CardanoWalletApiConnectionError)
import           PlutusAppsExtra.Types.Error          (ConnectionError (..))
import           Servant                              (Proxy (..), hoistServer, serve)
import qualified Servant
import           System.IO                            (BufferMode (LineBuffering), hSetBuffering, stdout)
import           UnliftIO.Concurrent                  (forkIO)

data RunSettings api = RunSettings
    { rsBeforeMainLoop      :: AppT api IO ()
    , rsServerDiagnostics   :: Int -> AppT api IO ()
    , rsDiagnosticsInterval :: Int
    , rsServerName          :: Text
    }

instance Default (RunSettings api) where
    def = RunSettings
        { rsBeforeMainLoop      = pure ()
        , rsServerDiagnostics   = flip doDiagnostics pingDiagnostics
        , rsDiagnosticsInterval = 600
        , rsServerName          = "server"
        }

type IsCardanoServer api =
    ( Servant.HasServer api '[]
    , FromJSON (AuxillaryConfigOf api)
    , GetEdpointNames api
    )

runServer :: forall api.
    ( HasCreds
    , Servant.HasServer api '[]
    ) => Servant.ServerT api (ServerM api) -> Env api -> RunSettings api -> IO ()
runServer server Env{..} RunSettings{..} = (`catches` errorHanlders) $ do
    hSetBuffering stdout LineBuffering
    runApp $ forkIO $ rsServerDiagnostics rsDiagnosticsInterval
    case (envHyperTextProtocol, ?creds) of
        (HTTP, _)                 -> Warp.runSettings settings app
        (HTTPS, Just (cert, key)) -> Warp.runTLS (Warp.tlsSettingsMemory cert key) settings app
        (HTTPS, Nothing)          -> error noCredsMsg
    where
        app = corsWithContentType $ serve (Proxy @api)
            $ hoistServer (Proxy @api) (runAppT @_ @Servant.Handler Env{..}) server
        runHandler = fmap (either throw id) . Servant.runHandler
        runApp :: AppT api m a -> m a
        runApp = runAppT Env{..}
        settings = Warp.setLogger logReceivedRequest
                 $ Warp.setOnException (const logException)
                 $ Warp.setPort envPort
                 $ Warp.setBeforeMainLoop beforeMainLoop
                   Warp.defaultSettings
        beforeMainLoop = runApp $ do
            logMsg ("Starting " <> rsServerName <> "...")
            rsBeforeMainLoop
        logReceivedRequest req status _ = runHandler $ runApp $
            logMsg $ "Received request:\n" .< req <> "\nStatus:\n" .< status
        logException = runHandler . runAppT Env{..} . logCriticalExceptions
        noCredsMsg = "No creds given to run with HTTPS. \
                     \Add key.pem and certificate.pem file before compilation. \
                     \If this message doesn't go away, try running `cabal clean` first."
        errorHanlders = [Handler connectionErroH]
        connectionErroH e = T.putStrLn $ (<> " is unavailable.") $ case e of
            PlutusChainIndexConnectionError{} -> "Cardano chain index"
            KupoConnectionError{}             -> "Kupo chain index"
            CardanoWalletApiConnectionError{} -> "Cardano wallet"
            ConnectionError req _             -> T.decodeUtf8 $ path req

-- Embed https cert and key files on compilation
{-# INLINABLE embedCreds #-}
embedCreds :: Creds
embedCreds =
    let keyCred  = $(embedFileIfExists "../key.pem" )
        certCred = $(embedFileIfExists "../certificate.pem")
    in (,) <$> certCred <*> keyCred

corsWithContentType :: Wai.Middleware
corsWithContentType = cors (const $ Just policy)
    where policy = simpleCorsResourcePolicy
            { corsRequestHeaders = ["Content-Type"]
            }