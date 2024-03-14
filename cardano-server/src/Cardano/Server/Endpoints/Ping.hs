{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Cardano.Server.Endpoints.Ping where

import           Cardano.Server.Handler      (wrapHandler)
import           Cardano.Server.Internal     (ServerM)
import           Cardano.Server.Utils.Logger (logMsg)
import           Servant                     (Get, JSON, NoContent (..), type (:>))

type PingApi = "ping" :> Get '[JSON] NoContent

pingHandler :: ServerM api NoContent
pingHandler = wrapHandler @PingApi $ do
    logMsg "Received ping request."
    pure NoContent