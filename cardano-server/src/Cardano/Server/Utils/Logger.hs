{-# LANGUAGE OverloadedStrings #-}

module Cardano.Server.Utils.Logger where

import           Control.Exception      (handle, throw)
import           Control.Monad.IO.Class (MonadIO(..))
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import qualified Data.Time              as Time
import           GHC.IO.Exception       (IOException(..), IOErrorType(NoSuchThing))
import           Prettyprinter          (Pretty(..))
import           System.Directory       (createDirectoryIfMissing)
import           System.FilePath.Posix  (takeDirectory)

type Logger m = (Text -> m ())

class MonadIO m => HasLogger m where
    getLogger :: m (Logger m)

    getLoggerFilePath :: m (Maybe FilePath)
    getLoggerFilePath = pure Nothing

instance HasLogger IO where
    getLogger = pure T.putStrLn

logger :: HasLogger m => Logger m
logger msg = getLoggerFilePath >>= liftIO . logMsgIO msg

mutedLogger :: Monad m => (Logger m)
mutedLogger = const $ pure ()

logMsg :: HasLogger m => Text -> m ()
logMsg msg = getLogger >>= ($ msg)

logSmth :: (HasLogger m, Show a) => a -> m ()
logSmth a = logMsg $ T.pack $ show a

logPretty :: (HasLogger m, Pretty a) => a -> m ()
logPretty a = logMsg $ T.pack $ show $ pretty a

logMsgIO :: Text -> Maybe FilePath -> IO ()
logMsgIO msg fileName = handle (maybe (const $ pure ()) (handler msg) fileName) $ do
    utcTime <- Time.getCurrentTime
    let fTime = Time.formatTime Time.defaultTimeLocale "%a %b %d %H:%M:%S %Y" utcTime <> " UTC"
        msg' = "\n" <> T.pack fTime <> "\n" <> msg <> "\n"
    T.putStrLn msg'
    maybe (pure ()) ((`T.appendFile` msg') . mkFullPath) fileName

handler :: Text -> FilePath -> IOException -> IO ()
handler msg fileName err
    | ioe_type err == NoSuchThing = do
        createDirectoryIfMissing True $ takeDirectory $ mkFullPath fileName
        logMsgIO msg $ Just fileName
    | otherwise = throw err

mkFullPath :: FilePath -> FilePath
mkFullPath = ("logs/" <>)

(.<) :: (Show a) => T.Text -> a -> T.Text
text .< a = text <> "\n" <> T.pack (show a)

infixr 7 .<