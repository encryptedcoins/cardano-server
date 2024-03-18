module Cardano.Server.Handler where

import           Cardano.Server.EndpointName  (GetEndpointName, checkThatEndpointIsActive)
import           Cardano.Server.Error.Class   (IsCardanoServerError, toServantError)
import           Cardano.Server.Error.Servant (EndpointErrors)
import           Cardano.Server.Error.Utils   (All)
import           Cardano.Server.Internal      (Env (envActiveEndpoints), ServerM)
import           Cardano.Server.Utils.Logger  (HasLogger, logSmth)
import           Control.Exception            (SomeException)
import           Control.Monad.Catch          (MonadCatch, handle)
import           Control.Monad.Except         (MonadError)
import           Control.Monad.Reader.Class   (asks)
import           Data.Kind                    (Type)
import           GHC.TypeLits                 (KnownSymbol)
import           Servant                      (ServerError, throwError)

-- Catch all errors, specified in endpoint and throw corresponding
-- servant error instead. Also checks that endpoint haven't turned off
-- in config
wrapHandler :: forall e api a.
    ( KnownSymbol (GetEndpointName e)
    , WithErrorHandlers (EndpointErrors e)
    ) => ServerM api a -> ServerM api a
wrapHandler handler = withErrorHandlers @(EndpointErrors e) $ do
    asks envActiveEndpoints >>= checkThatEndpointIsActive @e
    handler

class All IsCardanoServerError es => WithErrorHandlers (es :: [Type]) where
    withErrorHandlers :: (MonadCatch m,  HasLogger m, MonadError ServerError m) => m a -> m a

instance WithErrorHandlers '[] where
    withErrorHandlers = handle $ \(e :: SomeException) -> logSmth e >> throwError (toServantError e)

instance (All IsCardanoServerError (e ': es), WithErrorHandlers es) => WithErrorHandlers (e ': es) where
    withErrorHandlers = withErrorHandlers @es . handle reThrow
        where
            reThrow e = logSmth e >> throwError (toServantError @e e)

-- class ApplyToHandler a a0 where
--     applyToHandler :: a -> (a0 -> a0) -> a

-- instance ApplyToHandler a a where
--     applyToHandler h fun = fun h

-- instance ApplyToHandler a a0 => ApplyToHandler (b -> a) a0 where
--     applyToHandler h fun = \x -> applyToHandler (h x) fun

-- instance ApplyToHandler a0 a0 where
--     applyToHandler' = ($)

-- instance ApplyToHandler aN aM => ApplyToHandler (a -> aN) aM where
--     applyToHandler' f h = applyToHandler' f . h