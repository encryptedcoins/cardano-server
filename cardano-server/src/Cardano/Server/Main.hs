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
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Cardano.Server.Main where

import           Cardano.Server.Config                (CardanoServerConfig (..), Creds, HasCreds, HyperTextProtocol (..))
import           Cardano.Server.Error                 (ConnectionError (..), logCriticalExceptions)
import           Cardano.Server.Internal              (Env, ServerM (unServerM))
import           Cardano.Server.Utils.Logger          (HasLogger, logMsg, (.<))
import           Control.Concurrent                   (forkIO)
import           Control.Exception                    (throw)
import           Control.Monad                        (void)
import           Control.Monad.Catch                  (handle)
import           Control.Monad.Reader                 (ReaderT (..))
import           Data.FileEmbed                       (embedFileIfExists)
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
import           Servant                              (Proxy (..), ServerT, hoistServer, serve)
import qualified Servant
import           System.IO                            (BufferMode (LineBuffering), hSetBuffering, stdout)

runServer :: forall api.
    ( Servant.HasServer api '[]
    , HasCreds
    ) => Env api -> ServerT api (ServerM api) -> ServerM api () -> IO ()
runServer env serverApp beforeMainLoop = handle connectionErroH $ runCardanoServer @api env (runApp env) serverApp beforeMainLoop
  where
    connectionErroH e = T.putStrLn $ (<> " is unavailable.") $ case e of
        PlutusChainIndexConnectionError{} -> "Cardano chain index"
        KupoConnectionError{}             -> "Kupo chain index"
        CardanoWalletApiConnectionError{} -> "Cardano wallet"
        ConnectionError req _             -> T.decodeUtf8 $ path req

runApp :: Env api -> ServerM api a -> Servant.Handler a
runApp env = (`runReaderT` env) . unServerM

runCardanoServer :: forall api m c.
    ( Servant.HasServer api '[]
    , HasLogger m
    , HasCreds
    , CardanoServerConfig c
    ) => c -> (forall a. m a -> Servant.Handler a) -> ServerT api m -> m () -> IO ()
runCardanoServer config runCardanoApp serverApp beforeMainLoop = handle connectionErrorH $ do
    hSetBuffering stdout LineBuffering
    case (configHyperTextProtocol config, ?creds) of
        (HTTP, _)                 -> Warp.runSettings settings app
        (HTTPS, Just (cert, key)) -> Warp.runTLS (Warp.tlsSettingsMemory cert key) settings app
        (HTTPS, Nothing)          -> error noCredsMsg
  where
    app = corsWithContentType $ serve (Proxy @api) $ hoistServer (Proxy @api) runCardanoApp serverApp
    runHandler = fmap (either throw id) . Servant.runHandler
    settings = Warp.setLogger logReceivedRequest
             $ Warp.setOnException (const logException)
             $ Warp.setPort (configPort config)
             $ Warp.setBeforeMainLoop (void $ forkIO $ runHandler $ runCardanoApp beforeMainLoop)
               Warp.defaultSettings
    logReceivedRequest req status _ = runHandler $ runCardanoApp $
        logMsg $ "Received request:\n" .< req <> "\nStatus:\n" .< status
    logException = runHandler . runCardanoApp . logCriticalExceptions
    noCredsMsg = "No creds given to run with HTTPS. \
                 \Add key.pem and certificate.pem file before compilation. \
                 \If this message doesn't go away, try running `cabal clean` first."
    connectionErrorH e = T.putStrLn $ (<> " is unavailable.") $ case e of
        PlutusChainIndexConnectionError{} -> "Cardano chain index"
        KupoConnectionError{}             -> "Kupo chain index"
        CardanoWalletApiConnectionError{} -> "Cardano wallet"
        ConnectionError req _             -> T.decodeUtf8 $ path req

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