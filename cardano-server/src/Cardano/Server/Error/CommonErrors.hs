{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# OPTIONS_GHC -Wno-orphans   #-}

module Cardano.Server.Error.CommonErrors
    ( InternalServerError (..)
    , ConnectionError (..)
    , BalanceExternalTxError (..)
    , MkTxError (..)
    , SubmitTxToLocalNodeError (..)
    , CslError (..)
    ) where

import           Cardano.Server.Error.Class           (IsCardanoServerError (errMsg, errStatus))
import           Cardano.Server.Utils.Logger          ((.<))
import           Control.Exception                    (Exception)
import           PlutusAppsExtra.IO.ChainIndex.Kupo   (pattern KupoConnectionError)
import           PlutusAppsExtra.IO.ChainIndex.Plutus (pattern PlutusChainIndexConnectionError)
import           PlutusAppsExtra.IO.Wallet            (pattern WalletApiConnectionError)
import           PlutusAppsExtra.Types.Error          (BalanceExternalTxError (..), ConnectionError (..), MkTxError (..),
                                                       SubmitTxToLocalNodeError (..))

data InternalServerError
    = NoWalletProvided
    deriving (Show, Exception)

instance IsCardanoServerError ConnectionError where
    errStatus _ = toEnum 503
    errMsg = \case
        PlutusChainIndexConnectionError{} -> toMsg "Caradno chain index API"
        KupoConnectionError{}             -> toMsg "Kupo chain index API"
        WalletApiConnectionError{}        -> toMsg "Cardano wallet API"
        _                                 -> toMsg "Some external API"
        where toMsg = (<> " is currently unavailable. Try again later.")

instance IsCardanoServerError MkTxError where
    errStatus _ = toEnum 422
    errMsg e = "The requested transaction could not be built. Reason:" .< e

instance IsCardanoServerError BalanceExternalTxError where
    errStatus _ = toEnum 422
    errMsg e = "The requested transaction could not be built. Reason: " <> case e of
        MakeUnbalancedTxError lookups constrs
            -> "Unable to build an UnbalancedTx:\n\nTxLookups:\n\n" <> lookups <> "\n\nTxConstraints:\n\n" <> constrs
        NonBabbageEraChangeAddress addr
            -> "Change address is not from Babbage era:\n\n" .< addr
        MakeUtxoProviderError tx err
            -> "Unable to extract an utxoProvider from wallet outputs:\n\nFailed tx:\n\n" .< tx <> "\n\nError:\n\n" .< err
        MakeAutoBalancedTxError tx err
            -> "Unable to build an auto balanced tx:\n\nFailed tx:\n\n" .< tx <> "\n\nError:\n\n" .< err

instance IsCardanoServerError SubmitTxToLocalNodeError where
    errStatus = \case
        NoConnectionToLocalNode -> toEnum 503
        _                       -> toEnum 422
    errMsg = \case
        NoConnectionToLocalNode -> "Server local node is currently unavailable."
        FailedSumbit err        -> "An error occurred while sending tx to local node. Reason: " .< err

data CslError
    = CslConversionError
    deriving (Show, Exception)

instance IsCardanoServerError CslError where
    errStatus _ = toEnum 422
    errMsg  = \case
        CslConversionError -> "An error occurred while converting plutus data to csl."