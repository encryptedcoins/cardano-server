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
import           Cardano.Server.Config          (Config (..), HasCreds)
import           Cardano.Server.Internal        (ServerHandle, loadEnv, runServerM, setLoggerFilePath, mkServantClientEnv)
import           Cardano.Server.Utils.Logger    (logMsg, (.<))
import           Control.Exception              (handle)
import           Control.Monad.IO.Class         (MonadIO (..))
import           Control.Monad.Reader           (void)

-- | When client options ~ CommonOptions
runClient :: HasCreds => Config -> ServerHandle api -> ClientHandle api -> IO ()
runClient c sh ch = runWithOpts >>= runClientWithOpts c sh ch

-- | For clients with another options type
runClientWithOpts :: HasCreds => Config -> ServerHandle api -> ClientHandle api -> CommonOptions -> IO ()
runClientWithOpts c sh ClientHandle{..} CommonOptions{..} = handleNotImplementedMethods $ do
    env         <- loadEnv c sh
    sce         <- liftIO $ mkServantClientEnv (cPort c) (cHost c) (cHyperTextProtocol c)
    let ?servantClientEnv = sce
    runServerM env $ setLoggerFilePath "client.log" $ withGreetings $ case (optsMode, optsEndpoint) of
        (Auto     i, "ping"    ) -> void $ autoPing         i
        (Auto     i, "utxos"   ) -> void $ autoUtxos        i
        (Auto     i, "newTx"   ) -> void $ autoNewTx        i
        (Auto     i, "submitTx") -> void $ autoSumbitTx     i
        (Auto     i, "serverTx") -> void $ autoServerTx     i
        (Auto     i, "version" ) -> void $ autoVersion      i
        (Manual txt, "ping"    ) -> void $ manualPing     txt
        (Manual txt, "utxos"   ) -> void $ manualUtxos    txt
        (Manual txt, "newTx"   ) -> void $ manualNewTx    txt
        (Manual txt, "submitTx") -> void $ manualSubmitTx txt
        (Manual txt, "serverTx") -> void $ manualServerTx txt
        (Manual txt, "version" ) -> void $ manualVersion  txt
        _ -> error "unknown endpoint"
    where
        withGreetings = (logMsg "Starting client..." >>)
        handleNotImplementedMethods = handle $ \(NotImplementedMethodError mode endpoint) ->
            let mode' = case mode of {Auto _ -> "auto"; Manual _ -> "manual"}
            in logMsg $ "You are about to use a function from client handle for which you did not provide an implementation:\n"
                <> mode' .< endpoint