{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

module Cardano.Server.Endpoints.Version where

import           Cardano.Server.Internal (ServerM)
import           Data.Text               (Text)
import           Data.Version            (Version)
import           Encoins.Common.Version  (AppVersion, appVersion)
import           Servant                 (Get, JSON, type (:>))

type VersionApi = "version" :> Get '[JSON] AppVersion

versionEndpointHandler :: Version -> Text -> String -> ServerM api AppVersion
versionEndpointHandler version commit time = pure $ appVersion version commit time
