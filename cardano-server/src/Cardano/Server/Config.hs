{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE ImplicitParams       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Server.Config where

import           Cardano.Server.Utils.Logger ((.<))
import           Data.Aeson                  (FromJSON (..), ToJSON, Value (Object), eitherDecodeFileStrict, withObject, (.:), (.:?))
import           Data.ByteString             (ByteString)
import           Data.Data                   (Typeable, typeOf)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.IO                as T
import           GHC.Generics                (Generic)
import           GHC.Stack                   (HasCallStack)
import qualified Servant.Client              as Servant

data Config api = Config
    { cHost              :: Text
    , cPort              :: Int
    , cHyperTextProtocol :: HyperTextProtocol
    , cActiveEndpoints   :: Maybe [Text]
    , cAuxilaryConfig    :: AuxillaryConfigOf api
    } deriving (Generic)

type family AuxillaryConfigOf api

deriving instance Show (AuxillaryConfigOf api) => Show (Config api)
deriving instance Eq   (AuxillaryConfigOf api) => Eq   (Config api)

instance (FromJSON (AuxillaryConfigOf api)) => FromJSON (Config api) where
   parseJSON = withObject "Cardano server config" $ \o -> do
        cHost              <- o .:  "host"
        cPort              <- o .:  "port"
        cHyperTextProtocol <- o .:  "hyper_text_protocol"
        cActiveEndpoints   <- o .:? "active_endpoints"
        cAuxilaryConfig    <- parseJSON $ Object o
        pure Config{..}

decodeOrErrorFromFile :: (HasCallStack, FromJSON a) => FilePath -> IO a
decodeOrErrorFromFile = fmap (either error id) . eitherDecodeFileStrict

-- Useful function for initializing optional env fields
withDefault :: (Typeable a, Show a) => a -> Maybe a -> IO a
withDefault defVal mbVal = (\d -> maybe d pure mbVal) $ do
    T.putStrLn $ "No " <> T.pack (show (typeOf defVal)) <> " specified. Using default:" .< defVal
    pure defVal

data HyperTextProtocol = HTTP | HTTPS
    deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

type Creds = Maybe (ByteString, ByteString)

type HasCreds = ?creds :: Creds

schemeFromProtocol :: HyperTextProtocol -> Servant.Scheme
schemeFromProtocol = \case
    HTTP  -> Servant.Http
    HTTPS -> Servant.Https