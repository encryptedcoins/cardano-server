module Cardano.Server.Error
    -- Error combinators
    ( Throws
    , Envelope
    , toEnvelope
    , IsCardanoServerError(..)

    -- Cardano server errors
    , ConnectionError (..)
    , BalanceExternalTxError (..)
    , MkTxError (..)
    , SubmitTxToLocalNodeError (..)

    -- Other helpers functions
    , throwMaybe
    , throwEither
    ) where

import           Cardano.Server.Error.Class      (IsCardanoServerError (..))
import           Cardano.Server.Error.Servant    (Envelope, Throws)
import           Cardano.Server.Error.ToEnvelope (toEnvelope)
import           PlutusAppsExtra.Types.Error     (ConnectionError(..), BalanceExternalTxError(..), SubmitTxToLocalNodeError(..), MkTxError(..),
                                                  throwMaybe, throwEither)