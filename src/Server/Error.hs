{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}

module Server.Error where

import           Data.Text           (Text)
import           Control.Monad.Catch (Exception(..), MonadThrow(..), handle)
import           IO.ChainIndex       (pattern ChainIndexConnectionError)
import           IO.Wallet           (pattern WalletApiConnectionError)
import           Server.Internal     (NetworkM)
import           Servant             (WithStatus, IsMember, Union)
import           Utils.Servant       (respondWithStatus)

handleUnavailableEndpoints :: forall s res. (IsMember (WithStatus 503 Text) res) => 
    NetworkM s (Union res) -> NetworkM s (Union res)
handleUnavailableEndpoints = handle $ \err -> case fromException err of
        Just ChainIndexConnectionError -> mkRespond "chain index"
        Just WalletApiConnectionError  -> mkRespond "wallet"
        _                              -> throwM err
    where
        mkRespond = respondWithStatus @503 . mkMsg
        mkMsg api = "Cardano " <> api <> "api is currently unavailable. Try again later."