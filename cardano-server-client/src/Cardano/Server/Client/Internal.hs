{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -Wno-unused-foralls #-}

module Cardano.Server.Client.Internal where

import           Cardano.Server.Config                (ServerEndpoint (..))
import           Cardano.Server.Endpoints.Utxos       (UtxosApi)
import           Cardano.Server.Endpoints.Ping        (PingApi)
import           Cardano.Server.Endpoints.Status      (StatusApi')
import           Cardano.Server.Endpoints.Tx.Internal (TxApiErrorOf)
import           Cardano.Server.Endpoints.Tx.New      (NewTxApi)
import           Cardano.Server.Endpoints.Tx.Server   (ServerTxApi)
import           Cardano.Server.Endpoints.Tx.Submit   (SubmitTxApi, SubmitTxReqBody (..))
import           Cardano.Server.Internal              (HasStatusEndpoint (..), TxApiRequestOf)
import qualified CSL
import           Data.Kind                            (Type)
import           Data.Text                            (Text)
import           Servant                              (Post, JSON, MimeRender, NoContent, Proxy (Proxy))
import           Servant.Client                       (ClientM, HasClient, client)

pingC :: ClientM NoContent
pingC = client (Proxy @PingApi)

utxosC :: Text -> ClientM CSL.TransactionUnspentOutputs
utxosC = client (Proxy @UtxosApi)

newTxC :: forall api. MimeRender JSON (TxApiRequestOf api) => TxApiRequestOf api -> ClientM (Text, Text)
newTxC = client (Proxy @(NewTxApi (TxApiRequestOf api) (TxApiErrorOf api)))

submitTxC :: forall api. SubmitTxReqBody -> ClientM NoContent
submitTxC = client (Proxy @(SubmitTxApi (TxApiErrorOf api)))

serverTxC :: forall api. MimeRender JSON (TxApiRequestOf api) => TxApiRequestOf api -> ClientM NoContent
serverTxC = client (Proxy @(ServerTxApi (TxApiRequestOf api) (TxApiErrorOf api)))

statusC :: forall api. 
    ( MimeRender JSON (StatusEndpointReqBodyOf api)
    , HasClient ClientM (Post '[JSON] (StatusEndpointResOf api))
    ) => StatusEndpointReqBodyOf api -> ClientM (StatusEndpointResOf api)
statusC = client (Proxy @(StatusApi' api))

class (Show (EndpointArg e api), Show (EndpointRes e api)) => ClientEndpoint (e :: ServerEndpoint) api where
    type EndpointArg e api :: Type
    type EndpointRes e api :: Type
    endpointClient         :: EndpointArg e api -> ClientM (EndpointRes e api)

instance ClientEndpoint 'PingE api where
    type EndpointArg 'PingE _ = ()
    type EndpointRes 'PingE _ = NoContent
    endpointClient            = const pingC

instance ClientEndpoint 'UtxosE api where
    type EndpointArg 'UtxosE _ = Text
    type EndpointRes 'UtxosE _ = CSL.TransactionUnspentOutputs
    endpointClient             = utxosC

instance (Show (TxApiRequestOf api), MimeRender JSON (TxApiRequestOf api)) => ClientEndpoint 'NewTxE api where
    type EndpointArg 'NewTxE api = TxApiRequestOf api
    type EndpointRes 'NewTxE _   = (Text, Text)
    endpointClient               = newTxC @api

instance ClientEndpoint 'SubmitTxE api where
    type EndpointArg 'SubmitTxE api = SubmitTxReqBody
    type EndpointRes 'SubmitTxE _   = NoContent
    endpointClient                  = submitTxC

instance (Show (TxApiRequestOf api), MimeRender JSON (TxApiRequestOf api)) => ClientEndpoint 'ServerTxE api where
    type EndpointArg 'ServerTxE api = TxApiRequestOf api
    type EndpointRes 'ServerTxE _   = NoContent
    endpointClient                  = serverTxC @api

instance ( Show (StatusEndpointReqBodyOf api)
         , Show (StatusEndpointResOf api)
         , MimeRender JSON (StatusEndpointReqBodyOf api)
         , HasClient ClientM (Post '[JSON] (StatusEndpointResOf api))
         ) => ClientEndpoint 'StatusE api where
    type EndpointArg 'StatusE api = StatusEndpointReqBodyOf api
    type EndpointRes 'StatusE api = StatusEndpointResOf api
    endpointClient                = statusC @api 

type Interval = Int

data Mode
    = Auto   Interval
    | Manual Text
    deriving (Show, Eq)