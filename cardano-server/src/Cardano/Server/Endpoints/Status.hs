{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Cardano.Server.Endpoints.Status where

import           Cardano.Server.Config        (ServerEndpoint (StatusE))
import           Cardano.Server.Error.Servant (Throwing)
import           Cardano.Server.Internal      (Env (envServerHandle), HasStatusEndpoint (..), ServerHandle (shStatusHandler),
                                               StatusHandler, checkEndpointAvailability)
import           Cardano.Server.Utils.Logger  (logMsg, (.<))
import           Control.Monad.Reader         (asks)
import           Servant                      (Get, JSON, ReqBody, type (:>))

type StatusApi errors reqBody result = "status"
    :> Throwing errors
    :> ReqBody '[JSON] reqBody
    :> Get '[JSON] result

type StatusApi' api = StatusApi (StatusEndpointErrorsOf api) (StatusEndpointReqBodyOf api) (StatusEndpointResOf api)

commonStatusHandler :: Show (StatusEndpointReqBodyOf api) => StatusHandler api
commonStatusHandler reqBody = do
    logMsg $ "New status request received:\n" .< reqBody
    checkEndpointAvailability StatusE
    asks (shStatusHandler . envServerHandle) >>= ($ reqBody)
