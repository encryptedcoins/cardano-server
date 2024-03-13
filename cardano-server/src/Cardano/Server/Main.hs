{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImplicitParams        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Cardano.Server.Main where

import           Cardano.Server.Config                (Creds, HasCreds,
                                                       HyperTextProtocol (..))
import           Cardano.Server.Diagnostics           (doDiagnostics)
import           Cardano.Server.Error                 (ConnectionError (..),
                                                       logCriticalExceptions)
import           Cardano.Server.Internal              (Env (..), ServerM (..),
                                                       runServerM)
import           Cardano.Server.Utils.Logger          (logMsg, (.<))
import           Control.Concurrent                   (forkIO)
import           Control.Exception                    (Handler (Handler),
                                                       catches)
import           Control.Monad.Reader                 (ReaderT (runReaderT))
import           Data.FileEmbed                       (embedFileIfExists)
import qualified Data.Text.Encoding                   as T
import qualified Data.Text.IO                         as T
import           Network.HTTP.Client                  (path)
import qualified Network.Wai                          as Wai
import qualified Network.Wai.Handler.Warp             as Warp
import qualified Network.Wai.Handler.WarpTLS          as Warp
import           Network.Wai.Middleware.Cors          (CorsResourcePolicy (..),
                                                       cors,
                                                       simpleCorsResourcePolicy)
import           PlutusAppsExtra.Api.Kupo             (pattern KupoConnectionError)
import           PlutusAppsExtra.IO.ChainIndex.Plutus (pattern PlutusChainIndexConnectionError)
import           PlutusAppsExtra.IO.Wallet.Cardano    (pattern CardanoWalletApiConnectionError)
import           Servant                              (Proxy (..), ServerT,
                                                       hoistServer, serve)
import qualified Servant
import           System.IO                            (BufferMode (LineBuffering),
                                                       hSetBuffering, stdout)

runCardanoServer :: forall api.
    ( Servant.HasServer api '[]
    , HasCreds
    ) => Env api -> ServerT api (ServerM api) -> ServerM api () -> ServerM api () -> IO ()
runCardanoServer env@Env{..} serverApp diagnostics beforeMainLoop = (`catches` errorHanlders) $ do
    hSetBuffering stdout LineBuffering
    case (envHyperTextProtocol, ?creds) of
        (HTTP, _)                 -> Warp.runSettings settings app
        (HTTPS, Just (cert, key)) -> Warp.runTLS (Warp.tlsSettingsMemory cert key) settings app
        (HTTPS, Nothing)          -> error noCredsMsg
    where
        app = corsWithContentType $ serve (Proxy @api) $ hoistServer (Proxy @api) (flip runReaderT env . unServerM @api) serverApp
        settings = Warp.setLogger logReceivedRequest
                 $ Warp.setOnException (const logException)
                 $ Warp.setPort envPort
                 $ Warp.setBeforeMainLoop beforeMainLoop'
                   Warp.defaultSettings
        beforeMainLoop' = do
            logMsg "Starting server..."
            forkIO $ runServerM env $ doDiagnostics envDiagnosticsInterval diagnostics
            runApp beforeMainLoop
        logReceivedRequest req status _ =
            logMsg $ "Received request:\n" .< req <> "\nStatus:\n" .< status
        logException = runApp . logCriticalExceptions
        noCredsMsg = "No creds given to run with HTTPS. \
                     \Add key.pem and certificate.pem file before compilation. \
                     \If this message doesn't go away, try running `cabal clean` first."
        errorHanlders = [Handler connectionErroH]
        connectionErroH e = T.putStrLn $ (<> " is unavailable.") $ case e of
            PlutusChainIndexConnectionError{} -> "Cardano chain index"
            KupoConnectionError{}             -> "Kupo chain index"
            CardanoWalletApiConnectionError{} -> "Cardano wallet"
            ConnectionError req _             -> T.decodeUtf8 $ path req
        runApp = runServerM env

-- Embed https cert and key files on compilation
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