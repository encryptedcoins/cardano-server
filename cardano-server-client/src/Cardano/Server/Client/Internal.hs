{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -Wno-unused-foralls #-}

module Cardano.Server.Client.Internal where

import           Cardano.Server.Endpoints.Funds       (Funds, FundsApi, FundsReqBody)
import           Cardano.Server.Endpoints.Ping        (PingApi)
import           Cardano.Server.Endpoints.Tx.Internal (TxApiErrorOf)
import           Cardano.Server.Endpoints.Tx.New      (NewTxApi)
import           Cardano.Server.Endpoints.Tx.Server   (ServerTxApi)
import           Cardano.Server.Endpoints.Tx.Submit   (SubmitTxApi, SubmitTxReqBody (..))
import           Cardano.Server.Error.Servant         (clientEnvelopeToEither)
import           Cardano.Server.Internal              (TxApiRequestOf)
import           Data.Kind                            (Type)
import           Data.Text                            (Text)
import           Servant                              (JSON, MimeRender, NoContent, Proxy (Proxy), ServerError)
import           Servant.Client                       (ClientM, client)

ping :: ClientM NoContent
ping = client (Proxy @PingApi)

funds :: FundsReqBody -> ClientM (Either ServerError Funds)
funds body = clientEnvelopeToEither <$> client (Proxy @FundsApi) body

newTx :: forall api. MimeRender JSON (TxApiRequestOf api) => TxApiRequestOf api -> ClientM (Either ServerError Text)
newTx body = clientEnvelopeToEither <$> client (Proxy @(NewTxApi (TxApiRequestOf api) (TxApiErrorOf api))) body

submitTx :: forall api. SubmitTxReqBody -> ClientM (Either ServerError NoContent)
submitTx body = clientEnvelopeToEither <$> client (Proxy @(SubmitTxApi (TxApiErrorOf api))) body

serverTx :: forall api. MimeRender JSON (TxApiRequestOf api) => TxApiRequestOf api -> ClientM (Either ServerError NoContent)
serverTx body = clientEnvelopeToEither <$> client (Proxy @(ServerTxApi (TxApiRequestOf api) (TxApiErrorOf api))) body

data ServerEndpoint
    = PingE
    | FundsE
    | NewTxE
    | SubmitTxE
    | ServerTxE

instance Read ServerEndpoint where
    readsPrec _ = \case
        "ping"     -> [(PingE    , "")]
        "funds"    -> [(FundsE   , "")]
        "newTx"    -> [(NewTxE   , "")]
        "submitTx" -> [(SubmitTxE, "")]
        "serverTx" -> [(ServerTxE, "")]
        _          -> []

instance Show ServerEndpoint where
    show = \case
        PingE     -> "ping"
        FundsE    -> "funds"
        NewTxE    -> "newTx"
        SubmitTxE -> "submitTx"
        ServerTxE -> "serverTx"

class (Show (EndpointArg e api), Show (EndpointRes e)) => ClientEndpoint (e :: ServerEndpoint) api where
    type EndpointArg e api :: Type
    type EndpointRes e     :: Type
    endpointClient         :: EndpointArg e api -> ClientM (Either ServerError (EndpointRes e))

instance ClientEndpoint 'PingE api where
    type EndpointArg 'PingE _ = ()
    type EndpointRes 'PingE   = NoContent
    endpointClient            = fmap Right <$> const ping

instance ClientEndpoint 'FundsE api where
    type EndpointArg 'FundsE _ = FundsReqBody
    type EndpointRes 'FundsE   = Funds
    endpointClient             = funds

instance (Show (TxApiRequestOf api), MimeRender JSON (TxApiRequestOf api)) => ClientEndpoint 'NewTxE api where
    type EndpointArg 'NewTxE api = TxApiRequestOf api
    type EndpointRes 'NewTxE     = Text
    endpointClient               = newTx @api

instance ClientEndpoint 'SubmitTxE api where
    type EndpointArg 'SubmitTxE api = SubmitTxReqBody
    type EndpointRes 'SubmitTxE     = NoContent
    endpointClient                  = submitTx

instance (Show (TxApiRequestOf api), MimeRender JSON (TxApiRequestOf api)) => ClientEndpoint 'ServerTxE api where
    type EndpointArg 'ServerTxE api = TxApiRequestOf api
    type EndpointRes 'ServerTxE     = NoContent
    endpointClient                  = serverTx @api

type Interval = Int

data Mode
    = Auto   Interval
    | Manual Text
    deriving Show