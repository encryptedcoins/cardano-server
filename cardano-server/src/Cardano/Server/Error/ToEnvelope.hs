{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE UndecidableSuperClasses   #-}

module Cardano.Server.Error.ToEnvelope where

import           Cardano.Server.Error.Servant (Envelope (..))
import           Cardano.Server.Error.Utils   (All)
import           Control.Monad.Catch          (MonadCatch, handle)
import           Control.Monad.Except         (MonadError)
import           Data.Kind                    (Type)
import           Servant                      (ServerError, throwError)
import           Cardano.Server.Error.Class   (IsCardanoServerError, IsCardanoServerError, toServantError)

toEnvelope :: forall es m a.
    (MonadCatch m, MonadError ServerError m, WithErrorHandlers es) => m a -> m (Envelope es a)
toEnvelope = withErrorHandlers @es . fmap SuccEnvelope

class All IsCardanoServerError es => WithErrorHandlers (es :: [Type]) where
    withErrorHandlers :: (MonadCatch m, MonadError ServerError m) => m (Envelope es' a) -> m (Envelope es' a)

instance WithErrorHandlers '[] where
    withErrorHandlers = id

instance (All IsCardanoServerError (e ': es), WithErrorHandlers es) => WithErrorHandlers (e ': es) where
    withErrorHandlers = handle (throwError . toServantError @e) . withErrorHandlers @es