{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Server.Client.Client where

import           Cardano.Server.Client.Handle   (ClientHandle (..), NotImplementedMethodError (..))
import           Cardano.Server.Client.Internal (Mode (..))
import           Cardano.Server.Client.Opts     (CommonOptions (..), runWithOpts)
import           Cardano.Server.Config          (Config (..), ServerEndpoint (..), HyperTextProtocol (..))
import           Cardano.Server.Internal        (ServerHandle, loadEnv, runServerM, setLoggerFilePath)
import           Cardano.Server.Utils.Logger    (logMsg, (.<))
import           Control.Exception              (handle)
import           Control.Monad.IO.Class         (MonadIO (..))
import           Control.Monad.Reader           (void)
import qualified Data.Text                      as T
import           Network.HTTP.Client            (ManagerSettings (managerResponseTimeout), defaultManagerSettings, newManager,
                                                 responseTimeoutMicro)
import           Servant.Client                 (BaseUrl (BaseUrl), ClientEnv (..), Scheme (Http, Https), defaultMakeClientRequest)

createServantClientEnv :: Config -> IO ClientEnv
createServantClientEnv Config{..} = do
    manager     <- newManager defaultManagerSettings {managerResponseTimeout = responseTimeoutMicro 120_000_000}
    pure $ ClientEnv
        manager
        (BaseUrl scheme (T.unpack cHost) cPort "")
        Nothing
        defaultMakeClientRequest
    where
        scheme = case cHyperTextProtocol of
            HTTP  -> Http
            HTTPS -> Https


-- | When client options ~ CommonOptions
runClient :: Config -> ServerHandle api -> ClientHandle api -> IO ()
runClient c sh ch = runWithOpts >>= runClientWithOpts c sh ch

-- | For clients with another options type
runClientWithOpts :: Config -> ServerHandle api -> ClientHandle api -> CommonOptions -> IO ()
runClientWithOpts c sh ClientHandle{..} CommonOptions{..} = handleNotImplementedMethods $ do
    env         <- loadEnv c sh
    sce         <- liftIO $ createServantClientEnv c
    let ?servantClientEnv = sce
    runServerM env $ setLoggerFilePath "client.log" $ withGreetings $ case (optsMode, optsEndpoint) of
        (Auto     i, PingE    ) -> void $ autoPing         i
        (Auto     i, UtxosE   ) -> void $ autoUtxos        i
        (Auto     i, NewTxE   ) -> void $ autoNewTx        i
        (Auto     i, SubmitTxE) -> void $ autoSumbitTx     i
        (Auto     i, ServerTxE) -> void $ autoServerTx     i
        (Auto     i, StatusE  ) -> void $ autoStatus       i
        (Auto     i, VersionE ) -> void $ autoVersion      i
        (Manual txt, PingE    ) -> void $ manualPing     txt
        (Manual txt, UtxosE   ) -> void $ manualUtxos    txt
        (Manual txt, NewTxE   ) -> void $ manualNewTx    txt
        (Manual txt, SubmitTxE) -> void $ manualSubmitTx txt
        (Manual txt, ServerTxE) -> void $ manualServerTx txt
        (Manual txt, StatusE  ) -> void $ manualStatus   txt
        (Manual txt, VersionE ) -> void $ manualVersion   txt
    where
        withGreetings = (logMsg "Starting client..." >>)
        handleNotImplementedMethods = handle $ \(NotImplementedMethodError mode endpoint) ->
            let mode' = case mode of {Auto _ -> "auto"; Manual _ -> "manual"}
            in logMsg $ "You are about to use a function from client handle for which you did not provide an implementation:\n"
                <> mode' .< endpoint