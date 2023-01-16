{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}

module Cardano.Server.Error where

import           Cardano.Server.Endpoints.Servant (respondWithStatus)
import           Cardano.Server.Internal          (NetworkM)
import           Control.Monad.Catch              (Exception(..), MonadThrow(..), handle)
import           Data.Text                        (Text)
import           IO.ChainIndex                    (pattern ChainIndexConnectionError)
import           IO.Wallet                        (pattern WalletApiConnectionError)
import           Servant                          (WithStatus, IsMember, Union)

handleUnavailableEndpoints :: forall s res. (IsMember (WithStatus 503 Text) res) => 
    NetworkM s (Union res) -> NetworkM s (Union res)
handleUnavailableEndpoints = handle $ \err -> case fromException err of
        Just ChainIndexConnectionError -> mkRespond "chain index"
        Just WalletApiConnectionError  -> mkRespond "wallet"
        _                              -> throwM err
    where
        mkRespond = respondWithStatus @503 . mkMsg
        mkMsg api = "Cardano " <> api <> "api is currently unavailable. Try again later."