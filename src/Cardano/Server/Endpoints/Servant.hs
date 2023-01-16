{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}

module Cardano.Server.Endpoints.Servant where

import Cardano.Server.Utils.Logger (HasLogger(..))
import Data.Text                   (Text)
import GHC.Base                    (Nat)
import Servant                     (respond, WithStatus(..), Union, IsMember)
import Servant.API.Status          (KnownStatus)

respondWithStatus :: forall (status :: Nat) res m. 
    ( IsMember (WithStatus status Text) res
    , KnownStatus status
    , HasLogger m
    ) => Text -> m (Union res)
respondWithStatus msg = do
    logMsg msg
    respond (WithStatus @status msg)