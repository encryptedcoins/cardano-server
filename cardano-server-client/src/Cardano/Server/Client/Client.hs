{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Cardano.Server.Client.Client where

import           Cardano.Server.Client.Opts       (Interval)
import           Cardano.Server.EndpointName      (GetEndpointName, getEndpointName)
import           Cardano.Server.Endpoints.Ping    (PingApi)
import           Cardano.Server.Endpoints.Version (ServerVersion, VersionApi)
import           Cardano.Server.Error.Servant     (EndpointEnvelope)
import           Cardano.Server.Internal          (AppT (..), mkServerClientEnv)
import           Cardano.Server.Utils.Logger      (logMsg)
import           Cardano.Server.Utils.Wait        (waitTime)
import           Control.Monad                    (forever)
import           Control.Monad.Catch              (MonadCatch, MonadThrow (..))
import           Control.Monad.IO.Class           (MonadIO (liftIO))
import           Data.Data                        (Proxy (..))
import qualified Data.Text                        as T
import           GHC.TypeLits                     (KnownSymbol)
import           Servant                          (NoContent)
import           Servant.Client                   (ClientEnv, ClientM, client, runClientM)
import           System.Random                    (randomRIO)

type HasServantClientEnv = ?servantClientEnv :: ClientEnv

sendRequest :: forall e api m. (Show (EndpointEnvelope e), KnownSymbol (GetEndpointName e), MonadIO m, MonadCatch m)
    => ClientM (EndpointEnvelope e) -> AppT api m (EndpointEnvelope e)
sendRequest endpointClient = do
    logMsg $ "Sending " <> getEndpointName @e <> " request"
    clientEnv <- mkServerClientEnv
    res <- liftIO (runClientM endpointClient clientEnv)
    logMsg $ "Received response:\n" <> either (T.pack . show) (T.pack . show) res
    either throwM pure res

manualClient :: forall e api m. (Show (EndpointEnvelope e), KnownSymbol (GetEndpointName e), MonadIO m, MonadCatch m)
    => ClientM (EndpointEnvelope e) -> AppT api m (EndpointEnvelope e)
manualClient = sendRequest @e

autoClient :: forall e api m. (Show (EndpointEnvelope e), KnownSymbol (GetEndpointName e), MonadIO m, MonadCatch m)
    => Interval -> ClientM (EndpointEnvelope e) -> AppT api m ()
autoClient i c = forever $ do
    sendRequest @e c
    waitTime =<< randomRIO (1, i * 2)

autoClientWith :: forall e api m b. (Show (EndpointEnvelope e), KnownSymbol (GetEndpointName e), MonadIO m, MonadCatch m)
    => AppT api m b -> Interval -> (b -> ClientM (EndpointEnvelope e)) -> AppT api m ()
autoClientWith gen i c = do
    arg <- gen
    autoClient @e i (c arg)

pingClient :: ClientM (EndpointEnvelope PingApi)
pingClient = client (Proxy @PingApi)

manualPing :: (MonadIO m, MonadCatch m) => AppT api m NoContent
manualPing = manualClient @PingApi pingClient

autoPing :: (MonadIO m, MonadCatch m) => Interval -> AppT api m ()
autoPing i = autoClient @PingApi i pingClient

versionClient :: ClientM ServerVersion
versionClient = client (Proxy @VersionApi)

manualVersion ::(MonadIO m, MonadCatch m) => AppT api m ServerVersion
manualVersion = manualClient @VersionApi versionClient

autoVersion :: (MonadIO m, MonadCatch m) => Interval -> AppT api m ()
autoVersion i = autoClient @VersionApi i versionClient