{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Cardano.Server.Endpoints.Ping where

import           Cardano.Server.Config       (ServerEndpoint (PingE))
import           Cardano.Server.Internal     (ServerM, checkEndpointAvailability)
import           Cardano.Server.Utils.Logger (logMsg)
import           Servant                     (type (:>), NoContent(..), JSON, Get)

type PingApi = "ping" :> Get '[JSON] NoContent

pingHandler :: ServerM api NoContent
pingHandler = do
    checkEndpointAvailability PingE
    logMsg "Received ping request."
    pure NoContent