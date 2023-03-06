{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# OPTIONS_GHC -Wno-unused-foralls #-}

module Cardano.Server.Client.Api where

import           Cardano.Server.Endpoints.Funds     (Funds, FundsApi, FundsReqBody)
import           Cardano.Server.Endpoints.Ping      (PingApi)
import           Cardano.Server.Endpoints.Tx.Class  (HasTxEndpoints (..))
import           Cardano.Server.Endpoints.Tx.New    (NewTxApi)
import           Cardano.Server.Endpoints.Tx.Server (ServerTxApi)
import           Cardano.Server.Endpoints.Tx.Submit (SubmitTxApi, SubmitTxReqBody)
import           Cardano.Server.Error.Servant       (ClientEnvelope)
import           Cardano.Server.Main                (port)
import           Data.Data                          (Proxy (..))
import           Data.Text                          (Text)
import           PlutusAppsExtra.Utils.Servant      (getFromEndpointOnPort)
import           Servant.API                        (JSON, MimeRender, NoContent)
import           Servant.Client.Internal.HttpClient (ClientM, client)

getFromEndpointCS :: ClientM a -> IO a
getFromEndpointCS = getFromEndpointOnPort port

ping :: ClientM NoContent
ping = client (Proxy @PingApi)

funds :: FundsReqBody -> ClientM (ClientEnvelope Funds)
funds = client (Proxy @FundsApi)

newTx :: forall s. MimeRender JSON (TxApiRequestOf s) => TxApiRequestOf s -> ClientM (ClientEnvelope Text)
newTx = client (Proxy @(NewTxApi s))

sumbitTx :: forall s. SubmitTxReqBody -> ClientM (ClientEnvelope NoContent)
sumbitTx = client (Proxy @(SubmitTxApi s))

serverTx :: forall s. MimeRender JSON (TxApiRequestOf s) => TxApiRequestOf s -> ClientM (ClientEnvelope NoContent)
serverTx = client (Proxy @(ServerTxApi s))