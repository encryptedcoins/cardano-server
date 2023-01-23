{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
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

import           Control.Monad.Catch              (Exception(..), MonadThrow(..), handle, MonadCatch, SomeException)
import qualified Data.ByteString.Lazy             as LBS
import           Data.Data                        (cast)
import           Data.Kind                        (Type, Constraint)
import           Data.Text                        (Text)
import qualified Data.Text.Encoding               as T
import           IO.ChainIndex                    (pattern ChainIndexConnectionError)
import           IO.Wallet                        (pattern WalletApiConnectionError)
import           Network.Wai                      (Middleware, responseLBS, ResponseReceived)
import           Network.HTTP.Types               (Status)
import           Servant.API.ContentTypes         (JSON, MimeRender(..), NoContent, PlainText)
import           Servant.Checked.Exceptions       (ErrStatus(..), toErrEnvelope, Envelope, IsMember, Contains,
                                                   toSuccEnvelope, Throws)
import           Types.Error                      (ConnectionError, MkTxError, BalanceExternalTxError(..))

---------------------------------------------------- Common errors ----------------------------------------------------

-- Unlike other server errors, these errors are defined in another packages
-- and can't be derived via ExceptionDeriving.
-- So it needs to be manually added to the middleware.
instance IsCardanoServerError ConnectionError where
    errStatus _ = toEnum 503
    errMsg = \case
        ChainIndexConnectionError{} -> toMsg "Caradno chain index API"
        WalletApiConnectionError{}  -> toMsg "Cardano wallet API"
        _                           -> toMsg "Some external endpoint"
        where toMsg = (<> " is currently unavailable. Try again later.")

instance IsCardanoServerError MkTxError where
    errStatus _ = toEnum 422
    errMsg _ = "The requested transaction could not be built."

instance IsCardanoServerError BalanceExternalTxError where
    errStatus _ = toEnum 422
    errMsg e = "The requested transaction could not be built. Reason: " <> case e of
        MakeUnbalancedTxError          
            -> "Unable to build an UnbalancedTx."
        MakeBuildTxFromEmulatorTxError 
            -> "Unable to extract CardanoBuildTx from EmulatorTx."
        NonBabbageEraChangeAddress     
            -> "Change address is not from Babbage era."
        MakeUtxoProviderError          
            -> "Unable to extract an utxoProvider from wallet outputs."
        MakeAutoBalancedTxError        
            -> "Unable to build an auto balanced tx."

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

pattern UnwrappedError :: IsCardanoServerError e => e -> SomeException
pattern UnwrappedError e <- (fromException -> Just e)

errorMW :: Middleware
errorMW baseApp req respond = handle handleServerException $ baseApp req respond
    where
        handleServerException (UnwrappedError e) = rethrowWithWrap @ConnectionError e

        handleServerException (UnwrappedError e) = rethrowWithWrap @MkTxError e

        handleServerException (UnwrappedError e) = rethrowWithWrap @BalanceExternalTxError e

        handleServerException (fromException -> Just (CardanoServerError cse))
            = let status  = errStatus cse
                  headers = []
                  content = LBS.fromStrict . T.encodeUtf8 $ errMsg cse
              in baseApp req $ const $ respond $ responseLBS status headers content

        handleServerException e = throwM e

        rethrowWithWrap :: forall e. IsCardanoServerError e => e -> IO ResponseReceived
        rethrowWithWrap e = handleServerException (toException $ CardanoServerError e)

class Exception e => IsCardanoServerError e where
    errStatus :: e -> Status
    errMsg    :: e -> Text

instance IsCardanoServerError e => ErrStatus e where
    toErrStatus = errStatus

-- Root of server exceptions hierarchy
data CardanoServerError = forall e. IsCardanoServerError e => CardanoServerError e
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

instance MimeRender PlainText (Envelope errs NoContent) where
    mimeRender _ _ = ""

instance MimeRender JSON (Envelope errs NoContent) where
    mimeRender _ _ = ""