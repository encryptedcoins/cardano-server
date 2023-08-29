{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Server.Client.Client where

import           Cardano.Server.Client.Handle   (ClientHandle (..), NotImplementedMethodError (..))
import           Cardano.Server.Client.Internal (Mode (..))
import           Cardano.Server.Client.Opts     (CommonOptions (..), runWithOpts)
import           Cardano.Server.Config          (Config (..), ServerEndpoint (..))
import           Cardano.Server.Internal        (ServerHandle, loadEnv, runServerM, setLoggerFilePath)
import           Cardano.Server.Utils.Logger    (logMsg, (.<))
import           Control.Exception              (handle)
import           Control.Monad.IO.Class         (MonadIO (..))
import           Control.Monad.Reader           (void)
import qualified Data.Text                      as T
import           Network.HTTP.Client            (defaultManagerSettings, newManager)
import           Servant.Client                 (BaseUrl (BaseUrl), ClientEnv (..), Scheme (Http), defaultMakeClientRequest)

createServantClientEnv :: Config -> IO ClientEnv
createServantClientEnv Config{..} = do
    manager     <- newManager defaultManagerSettings
    pure $ ClientEnv
        manager
        (BaseUrl Http (T.unpack cHost) cPort "")
        Nothing
        defaultMakeClientRequest

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
        (Manual txt, PingE    ) -> void $ manualPing     txt
        (Manual txt, UtxosE   ) -> void $ manualUtxos    txt
        (Manual txt, NewTxE   ) -> void $ manualNewTx    txt
        (Manual txt, SubmitTxE) -> void $ manualSubmitTx txt
        (Manual txt, ServerTxE) -> void $ manualServerTx txt
        (Manual txt, StatusE  ) -> void $ manualStatus   txt
    where
        withGreetings = (logMsg "Starting client..." >>)
        handleNotImplementedMethods = handle $ \(NotImplementedMethodError mode endpoint) ->
            let mode' = case mode of {Auto _ -> "auto"; Manual _ -> "manual"}
            in logMsg $ "You are about to use a function from client handle for which you did not provide an implementation:\n"
                <> mode' .< endpoint