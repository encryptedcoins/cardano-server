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
import           Cardano.Server.Internal        (ServerHandle, loadEnv, runServerM)
import           Cardano.Server.Main            (port)
import           Cardano.Server.Utils.Logger    (HasLogger (..), (.<))
import           Control.Exception              (handle)
import           Control.Monad.Reader           (void)
import           Network.HTTP.Client            (defaultManagerSettings, newManager)
import           Servant.Client                 (BaseUrl (BaseUrl), ClientEnv (..), Scheme (Http), defaultMakeClientRequest)

runClient :: ServerHandle api -> ClientHandle api -> IO ()
runClient sh ClientHandle{..} = handleNotImplementedMethods $ do
    Options{..} <- runWithOpts
    Config{..}  <- loadConfig
    env         <- loadEnv sh
    manager     <- newManager defaultManagerSettings
    let ?servantClientEnv = ClientEnv
            manager
            (BaseUrl Http "localhost" {- (T.unpack cServerAddress) -} port ""{- (show optsEndpoint) -})
            Nothing
            defaultMakeClientRequest
    runServerM env $ withGreetings $ case (optsMode, optsEndpoint) of
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