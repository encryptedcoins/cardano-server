module Cardano.Server.Error
    -- Error combinators
    ( Throws
    , Envelope
    , toEnvelope

    -- Error class
    , IsCardanoServerError (..)

    -- Cardano server errors
    , ConnectionError (..)
    , BalanceExternalTxError (..)
    , MkTxError (..)
    , SubmitTxToLocalNodeError (..)

    -- Other helpers functions
    , parseErrorText
    , throwMaybe
    , throwEither
    ) where

import           Cardano.Server.Error.Class      (IsCardanoServerError (..), parseErrorText)
import           Cardano.Server.Error.Servant    (Envelope, Throws)
import           Cardano.Server.Error.ToEnvelope (toEnvelope)
import           PlutusAppsExtra.Types.Error     (ConnectionError(..), BalanceExternalTxError(..), SubmitTxToLocalNodeError(..), MkTxError(..),
                                                  throwMaybe, throwEither)