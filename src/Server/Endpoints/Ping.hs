{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Server.Endpoints.Ping where

import           Utils.Logger       (HasLogger(logMsg))
import           Servant            (type (:>), NoContent(..), JSON, Get)
import           Server.Class       (NetworkM)

type PingApi = "relayRequestPing" :> Get '[JSON] NoContent

pingHandler :: NetworkM s NoContent
pingHandler = NoContent <$ logMsg "Received ping request."