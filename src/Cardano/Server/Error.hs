{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstrainedClassMethods    #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wno-orphans            #-}

module Cardano.Server.Error 
    ( ConnectionError
    , IsCardanoServerError(..)
    , ExceptionDeriving(..)
    , toEnvelope
    , errorMW
    , Throws
    , Envelope
    ) where

import           Control.Monad.Catch              (Exception(..), MonadThrow(..), handle, MonadCatch)
import           Data.Aeson                       (ToJSON(..))
import qualified Data.Aeson                       as J
import qualified Data.ByteString.Lazy             as LBS
import           Data.Data                        (cast)
import           Data.Kind                        (Type, Constraint)
import           Data.Text                        (Text)
import qualified Data.Text.Encoding               as T
import           IO.ChainIndex                    (pattern ChainIndexConnectionError)
import           IO.Wallet                        (pattern WalletApiConnectionError)
import           Network.Wai                      (Middleware, responseLBS)
import           Network.HTTP.Types               (Status)
import           Servant                          (NoContent)
import           Servant.Checked.Exceptions       (ErrStatus(..), toErrEnvelope, Envelope, IsMember, Contains,
                                                   toSuccEnvelope, Throws)
import           Utils.Servant                    (ConnectionError(..))

---------------------------------------------------- Common errors ----------------------------------------------------

-- Unlike other server errors, ConnectionError is defined in another package 
-- and can't be derived via ExceptionDeriving.
-- So it needs to be manually added to the middleware.
instance IsCardanoServerError ConnectionError where
    errStatus _ = toEnum 503
    errMsg = \case
        ChainIndexConnectionError{} -> toMsg "Caradno chain index API"
        WalletApiConnectionError{}  -> toMsg "Cardano wallet API"
        _                           -> toMsg "Some external endpoint"
        where toMsg = (<> " is currently unavailable. Try again later.")

instance ToJSON ConnectionError where
    toJSON _ = J.String "Connection error."

----------------------------------------- Helper newtype to exception deriving  -----------------------------------------

newtype ExceptionDeriving e = ExceptionDeriving e

deriving newtype instance Show e => Show (ExceptionDeriving e)
deriving newtype instance IsCardanoServerError e => IsCardanoServerError (ExceptionDeriving e)

instance (Exception e, IsCardanoServerError e) => Exception (ExceptionDeriving e) where
    toException = toException . CardanoServerError
    fromException x = do
        CardanoServerError e <- fromException x
        cast e

------------------------------------------- Server error class and middleware  ------------------------------------------

errorMW :: Middleware
errorMW baseApp req respond = handle handleServerException $ baseApp req respond
    where
        handleServerException (fromException @ConnectionError -> Just ce) 
            = handleServerException (toException $ CardanoServerError ce)

        handleServerException (fromException -> Just (CardanoServerError cse))
            = let status  = errStatus cse
                  headers = []
                  content = LBS.fromStrict . T.encodeUtf8 $ errMsg cse
              in baseApp req $ const $ respond $ responseLBS status headers content

        handleServerException e = throwM e

class Exception e => IsCardanoServerError e where
    errMsg    :: e -> Text
    errStatus :: e -> Status

instance IsCardanoServerError e => ErrStatus e where
    toErrStatus = errStatus

data CardanoServerError = forall e. (Exception e, IsCardanoServerError e) => CardanoServerError e
    deriving Exception

instance IsCardanoServerError CardanoServerError where
    errMsg (CardanoServerError e) = errMsg e
    errStatus (CardanoServerError e) = errStatus e

instance Show CardanoServerError where
    show (CardanoServerError e) = show e

----------------------------------------- Helper function for envelope wrapping ----------------------------------------- 

toEnvelope :: forall es m a. 
    (MonadCatch m, All Exception es, WithErrorHandlers es, Contains es es) => m a -> m (Envelope es a)
toEnvelope = withErrorHandlers @es . fmap toSuccEnvelope

class WithErrorHandlers (es :: [Type]) where
    withErrorHandlers :: (MonadCatch m, Contains es es', All Exception es) => m (Envelope es' a) -> m (Envelope es' a)

instance WithErrorHandlers '[] where
    withErrorHandlers = id

instance WithErrorHandlers es => WithErrorHandlers (e ': es) where
    withErrorHandlers = withErrorHandler @e . withErrorHandlers @es
    
withErrorHandler :: forall e es m a. (MonadCatch m, Exception e, IsMember e es) => m (Envelope es a) -> m (Envelope es a)
withErrorHandler = handle (pure . toErrEnvelope @e)

type family All (constr :: Type -> Constraint) (xs :: [Type]) :: Constraint where
    All _ '[]       = ()
    All c (x ': xs) = (c x, All c xs)

------------------------------------------------ Servant orphan instances ------------------------------------------------

instance ToJSON NoContent where
    toJSON _ = J.Null 