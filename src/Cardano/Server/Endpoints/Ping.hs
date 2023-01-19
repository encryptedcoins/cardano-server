{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Cardano.Server.Endpoints.Ping where

import           Cardano.Server.Internal     (NetworkM)
import           Cardano.Server.Utils.Logger (HasLogger(logMsg))
import           Servant                     (type (:>), NoContent(..), JSON, Get)

type PingApi = "ping" :> Get '[JSON] NoContent

pingHandler :: NetworkM s NoContent
pingHandler = NoContent <$ logMsg "Received ping request."