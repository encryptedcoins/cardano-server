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
    , CslError (..)

    -- Other helpers functions
    , logCriticalExceptions
    , parseErrorText
    , throwMaybe
    , throwEither
    ) where

import           Cardano.Server.Error.Class        (IsCardanoServerError (..), parseErrorText)
import           Cardano.Server.Error.CommonErrors (CslError (..), logCriticalExceptions)
import           Cardano.Server.Error.Servant      (Envelope, Throws)
import           Cardano.Server.Error.ToEnvelope   (toEnvelope)
import           PlutusAppsExtra.Types.Error       (BalanceExternalTxError (..), ConnectionError (..), MkTxError (..),
                                                    SubmitTxToLocalNodeError (..), throwEither, throwMaybe)
