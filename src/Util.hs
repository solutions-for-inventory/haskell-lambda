{-# LANGUAGE RankNTypes #-}
module Util where

import RIO
import Control.Monad.Trans.Maybe ( MaybeT(..) )
import Data.Aeson ( defaultOptions, Options(fieldLabelModifier) )
import Data.List ( head, tail )
import Data.Char ( toLower )
import Data.UUID
import System.Random
import Data.ByteArray (Bytes, convert)
import Data.Text.Encoding (decodeUtf8)
import System.IO ( hPutStrLn )

toJsonOptions :: Int -> Options
toJsonOptions prefixLength =
    defaultOptions
        { fieldLabelModifier = headToLower . drop prefixLength }
    where
        headToLower x = toLower (head x) : tail x

hoistMaybe :: Monad m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . return

newUUID :: IO UUID
newUUID = randomIO

flipMaybe :: Maybe a -> b -> (a -> b) -> b
flipMaybe mb error f = maybe error f mb

fromTextToBytes :: Text -> Bytes
fromTextToBytes = convert . encodeUtf8

fromBytesToText :: Bytes -> Text
fromBytesToText = decodeUtf8 . convert

exitWithErrorMessageAndCode :: String -> ExitCode -> IO a
exitWithErrorMessageAndCode str e = hPutStrLn stderr str >> exitWith e

exitWithErrorMessage :: String -> IO a
exitWithErrorMessage msg = exitWithErrorMessageAndCode msg (ExitFailure 2)
