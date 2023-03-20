{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Server.Client.Client where

import           Cardano.Server.Client.Handle   (ClientHandle (..), NotImplementedMethodError (NotImplementedMethodError))
import           Cardano.Server.Client.Internal (Mode (Auto, Manual), ServerEndpoint (..))
import           Cardano.Server.Client.Opts     (Options (..), runWithOpts)
import           Cardano.Server.Config          (Config (..), loadConfig)
import           Cardano.Server.Internal        (ServerHandle, loadEnv, runServerM, setLoggerFilePath)
import           Cardano.Server.Utils.Logger    (logMsg, (.<))
import           Control.Exception              (handle)
import           Control.Monad.IO.Class         (MonadIO (..))
import           Control.Monad.Reader           (void)
import qualified Data.Text                      as T
import           Network.HTTP.Client            (defaultManagerSettings, newManager)
import           Servant.Client                 (BaseUrl (BaseUrl), ClientEnv (..), Scheme (Http), defaultMakeClientRequest)

createServantClientEnv :: IO ClientEnv
createServantClientEnv = do
    Config{..}  <- loadConfig
    manager     <- newManager defaultManagerSettings
    pure $ ClientEnv
        manager
        (BaseUrl Http (T.unpack cHost) cPort "")
        Nothing
        defaultMakeClientRequest

runClient :: ServerHandle api -> ClientHandle api -> IO ()
runClient sh ClientHandle{..} = handleNotImplementedMethods $ do
    Options{..} <- runWithOpts
    env         <- loadEnv sh
    sce         <- liftIO createServantClientEnv
    let ?servantClientEnv = sce
    runServerM env $ setLoggerFilePath "client.log" $ withGreetings $ case (optsMode, optsEndpoint) of
        (Auto     i, PingE    ) -> void $ autoPing         i
        (Auto     i, FundsE   ) -> void $ autoFunds        i
        (Auto     i, NewTxE   ) -> void $ autoNewTx        i
        (Auto     i, SubmitTxE) -> void $ autoSumbitTx     i
        (Auto     i, ServerTxE) -> void $ autoServerTx     i
        (Manual txt, PingE    ) -> void $ manualPing     txt
        (Manual txt, FundsE   ) -> void $ manualFunds    txt
        (Manual txt, NewTxE   ) -> void $ manualNewTx    txt
        (Manual txt, SubmitTxE) -> void $ manualSubmitTx txt
        (Manual txt, ServerTxE) -> void $ manualServerTx txt
    where
        withGreetings = (logMsg "Starting client..." >>)
        handleNotImplementedMethods = handle $ \(NotImplementedMethodError mode endpoint) ->
            let mode' = case mode of {Auto _ -> "auto"; Manual _ -> "manual"}
            in logMsg $ "You are about to use a function from client handle for which you did not provide an implementation:\n"
                <> mode' .< endpoint