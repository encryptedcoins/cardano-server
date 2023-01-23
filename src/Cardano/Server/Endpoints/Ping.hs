{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Cardano.Server.Endpoints.Ping where

import           Cardano.Server.Config       (isInactivePing)
import           Cardano.Server.Internal     (NetworkM, checkEndpointAvailability)
import           Cardano.Server.Utils.Logger (HasLogger(logMsg))
import           Servant                     (type (:>), NoContent(..), JSON, Get)

type PingApi = "ping" :> Get '[JSON] NoContent

pingHandler :: NetworkM s NoContent
pingHandler = checkEndpointAvailability isInactivePing 
    >> NoContent <$ logMsg "Received ping request."