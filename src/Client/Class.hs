{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Client.Class where

import           Data.Kind            (Type)
import           Control.Monad.Reader (asks, MonadIO, MonadReader, ReaderT(..))
import           IO.Wallet            (HasWallet(..), RestoredWallet)
import           Options.Applicative  (Parser)
import qualified Server.Class         as Server
import           Utils.Logger         (HasLogger(..))

newtype ClientM s a = ClientM { unClientM :: ReaderT (Env s) IO a }
    deriving newtype (Functor, Applicative, Monad, MonadReader (Env s), MonadIO)

runClientM :: Server.AuxiliaryEnvOf s -> RestoredWallet -> ClientM s a -> IO a
runClientM aEnv wallet = flip runReaderT (Env aEnv wallet) . unClientM

data Env s = Env
    { envAuxiliary :: Server.AuxiliaryEnvOf s
    , envWallet    :: RestoredWallet
    }

instance HasLogger (ClientM s) where
    loggerFilePath = "client.log"

instance HasWallet (ClientM s) where
    getRestoredWallet = asks envWallet

class ( Server.HasServer c
      , Show (RequestTermOf c)
      , Eq (RequestTermOf c)
      ) => HasClient c where

    type RequestTermOf c :: Type

    parseRequestTerm :: Parser (RequestTermOf c)

    genRequestTerm :: IO (RequestTermOf c)

    -- here ClientM c () are some additional actions, that would be executed
    -- on successful response
    mkRedeemer :: ClientRequestOf c -> ClientM c (ClientM c (), Server.InputOf c)

type ClientRequestOf s = [RequestTermOf s]