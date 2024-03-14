{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Cardano.Server.Endpoints.Version where

import           Servant                       (Get, JSON, type (:>))

import           Cardano.Server.Handler        (wrapHandler)
import           Cardano.Server.Internal
import           Data.Aeson                    (FromJSON (..), ToJSON (..))
import           Data.Maybe                    (fromMaybe)
import           Data.Text                     (Text, pack)
import qualified Data.Text                     as T
import           Data.Time                     (UTCTime, defaultTimeLocale, formatTime)
import           Data.Time.Clock.POSIX         (posixSecondsToUTCTime)
import           Data.Time.Format              (parseTimeM)
import           Data.Version                  (Version, showVersion)
import           GHC.Generics                  (Generic)
import           Prettyprinter                 (Pretty (pretty), annotate, defaultLayoutOptions, layoutSmart)
import           Prettyprinter.Render.Terminal (Color (Blue, Green), bold, color, renderStrict)

type VersionApi = "version"
    :> Get '[JSON] ServerVersion

versionEndpointHandler :: Version -> Text -> String -> ServerM api ServerVersion
versionEndpointHandler svVersion svCommit commitDate = wrapHandler @VersionApi $
    pure ServerVersion{..}
  where
    svDate = parseSvDate commitDate

parseSvDate :: String -> UTCTime
parseSvDate commitDate = fromMaybe (posixSecondsToUTCTime $ toEnum 0) $ parseTimeM
    False
    defaultTimeLocale
    "%a %b %e %T %Y %Z"
    commitDate

data ServerVersion = ServerVersion
  { svVersion :: Version
  , svCommit  :: Text
  , svDate    :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

showServerVersion :: ServerVersion -> String
showServerVersion sv = T.unpack $ T.intercalate "\n" [sVersion, sHash, sDate]
    where
        sVersion = textToColorText green $ "Encoins-relay-server " <> "v" <> T.pack (showVersion $ svVersion sv)
        sHash = " ➤ " <> textToColorText blue ("Git revision: " :: T.Text) <> svCommit sv
        sDate = " ➤ " <> textToColorText blue ("Commit date:  " :: T.Text) <> dateFormated
        dateFormated = formatPollTime $ svDate sv
        textToColorText col txt = renderStrict $ layoutSmart defaultLayoutOptions $ col $ pretty txt
        green = annotate $ color Green <> bold
        blue = annotate $ color Blue <> bold

formatPollTime :: UTCTime -> Text
formatPollTime
  = pack
  . formatTime defaultTimeLocale "%e %B %Y, %R %Z"
