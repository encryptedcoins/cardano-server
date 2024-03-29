{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wno-unused-foralls #-}

module Cardano.Server.Client.Internal where

import qualified CSL
import           Cardano.Server.Endpoints.Ping        (PingApi)
import           Cardano.Server.Endpoints.Tx.Internal (TxApiErrorOf)
import           Cardano.Server.Endpoints.Tx.New      (NewTxApi)
import           Cardano.Server.Endpoints.Tx.Server   (ServerTxApi)
import           Cardano.Server.Endpoints.Tx.Submit   (SubmitTxApi, SubmitTxReqBody (..))
import           Cardano.Server.Endpoints.Utxos       (UtxosApi)
import           Cardano.Server.Endpoints.Version     (VersionApi)
import           Cardano.Server.Internal              (TxApiRequestOf)
import           Data.Kind                            (Type)
import           Data.Text                            (Text)
import           Encoins.Common.Version               (AppVersion)
import           Servant                              (JSON, MimeRender, NoContent, Proxy (Proxy))
import           Servant.Client                       (ClientM, client)
import GHC.Base (Symbol)

pingC :: ClientM NoContent
pingC = client (Proxy @PingApi)

utxosC :: Text -> ClientM CSL.TransactionUnspentOutputs
utxosC = client (Proxy @UtxosApi)

newTxC :: forall api. MimeRender JSON (TxApiRequestOf api)
  => TxApiRequestOf api
  -> ClientM (Text, Text)
newTxC = client (Proxy @(NewTxApi (TxApiRequestOf api) (TxApiErrorOf api)))

submitTxC :: forall api. SubmitTxReqBody -> ClientM NoContent
submitTxC = client (Proxy @(SubmitTxApi (TxApiErrorOf api)))

serverTxC :: forall api. MimeRender JSON (TxApiRequestOf api)
  => TxApiRequestOf api
  -> ClientM NoContent
serverTxC = client (Proxy @(ServerTxApi (TxApiRequestOf api) (TxApiErrorOf api)))

versionC :: ClientM AppVersion
versionC = client (Proxy @VersionApi)

class (Show (EndpointArg e api), Show (EndpointRes e api)) => ClientEndpoint (e :: Symbol) api where
    type EndpointArg e api :: Type
    type EndpointRes e api :: Type
    endpointClient         :: EndpointArg e api -> ClientM (EndpointRes e api)

instance ClientEndpoint "ping" api where
    type EndpointArg "ping" _ = ()
    type EndpointRes "ping" _ = NoContent
    endpointClient            = const pingC

instance ClientEndpoint "utxos" api where
    type EndpointArg "utxos" _ = Text
    type EndpointRes "utxos" _ = CSL.TransactionUnspentOutputs
    endpointClient             = utxosC

instance (Show (TxApiRequestOf api), MimeRender JSON (TxApiRequestOf api)) => ClientEndpoint "newTx" api where
    type EndpointArg "newTx" api = TxApiRequestOf api
    type EndpointRes "newTx" _   = (Text, Text)
    endpointClient               = newTxC @api

instance ClientEndpoint "submitTx" api where
    type EndpointArg "submitTx" api = SubmitTxReqBody
    type EndpointRes "submitTx" _   = NoContent
    endpointClient                  = submitTxC

instance (Show (TxApiRequestOf api), MimeRender JSON (TxApiRequestOf api)) => ClientEndpoint "serverTx" api where
    type EndpointArg "serverTx" api = TxApiRequestOf api
    type EndpointRes "serverTx" _   = NoContent
    endpointClient                  = serverTxC @api

type Interval = Int

data Mode
    = Auto   Interval
    | Manual Text
    deriving (Show, Eq)
