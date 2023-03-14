{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Cardano.Server.Error.Class where

import           Control.Monad.Catch       (Exception)
import           Data.Aeson                (KeyValue ((.=)), (.:))
import qualified Data.Aeson                as J
import qualified Data.Aeson.Types          as J
import qualified Data.ByteString.Lazy      as LBS
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Network.HTTP.Types        (Status)
import qualified Network.HTTP.Types.Header
import qualified Servant

class Exception e => IsCardanoServerError e where

    errStatus :: e -> Status

    errStatusText :: e -> Text
    errStatusText = statusTextFromStatus . errStatus

    errMsg :: e -> Text

    errBody :: e -> LBS.ByteString
    errBody e = J.encode $ cardanoServerErrorToJSON e

    errHeaders :: e -> [Network.HTTP.Types.Header.Header]
    errHeaders _ = [(Network.HTTP.Types.Header.hContentType, "application/json")]

cardanoServerErrorToJSON :: IsCardanoServerError e => e -> J.Value
cardanoServerErrorToJSON e = J.object 
    [ "errCode" .= fromEnum (errStatus e)
    , "errMsg" .= errMsg e
    ]

cardanoServerErrorParser :: J.Value -> J.Parser Servant.ServerError
cardanoServerErrorParser = J.withObject "Cardano server error" $ \o -> do
    errHTTPCode     <- o .: "errCode"
    errReasonPhrase <- o .: "errMsg"
    pure $ Servant.ServerError errHTTPCode errReasonPhrase "" []
 
toServantError :: forall e. IsCardanoServerError e => e -> Servant.ServerError
toServantError e = Servant.ServerError 
    (fromEnum $ errStatus e) 
    (T.unpack $ errStatusText e)
    (errBody e) 
    (errHeaders e)

statusTextFromStatus :: Status -> Text
statusTextFromStatus s = case fromEnum s of
        300 -> getStatusText Servant.err300
        301 -> getStatusText Servant.err301
        302 -> getStatusText Servant.err302
        303 -> getStatusText Servant.err303
        304 -> getStatusText Servant.err304
        305 -> getStatusText Servant.err305
        307 -> getStatusText Servant.err307
        400 -> getStatusText Servant.err400
        401 -> getStatusText Servant.err401
        402 -> getStatusText Servant.err402
        403 -> getStatusText Servant.err403
        404 -> getStatusText Servant.err404
        405 -> getStatusText Servant.err405
        406 -> getStatusText Servant.err406
        407 -> getStatusText Servant.err407
        409 -> getStatusText Servant.err409
        410 -> getStatusText Servant.err410
        411 -> getStatusText Servant.err411
        412 -> getStatusText Servant.err412
        413 -> getStatusText Servant.err413
        414 -> getStatusText Servant.err414
        415 -> getStatusText Servant.err415
        416 -> getStatusText Servant.err416
        417 -> getStatusText Servant.err417
        418 -> getStatusText Servant.err418
        422 -> getStatusText Servant.err422
        500 -> getStatusText Servant.err500
        501 -> getStatusText Servant.err501
        502 -> getStatusText Servant.err502
        503 -> getStatusText Servant.err503
        504 -> getStatusText Servant.err504
        505 -> getStatusText Servant.err505
        _   -> ""
    where
        getStatusText = T.pack . Servant.errReasonPhrase
