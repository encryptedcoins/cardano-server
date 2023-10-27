{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Cardano.Server.Endpoints.Version where

import           Cardano.Server.Config       (ServerEndpoint (VersionE))
import           Cardano.Server.Internal     (Env (envServerHandle),
                                              HasVersionEndpoint (..),
                                              ServerHandle (shVersionHandler),
                                              VersionHandler,
                                              checkEndpointAvailability)
import           Cardano.Server.Utils.Logger (logMsg)
import           Control.Monad.Reader        (ask)
import           Servant                     (Get, JSON, type (:>))

type VersionApi result = "version"
    :> Get '[JSON] result

type VersionApi' api = VersionApi (VersionEndpointResOf api)

serverVersionHandler :: VersionHandler api
serverVersionHandler = do
    logMsg "Received version request."
    checkEndpointAvailability VersionE
    r <- ask
    shVersionHandler . envServerHandle $ r
