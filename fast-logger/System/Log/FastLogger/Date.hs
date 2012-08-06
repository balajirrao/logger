{-# LANGUAGE CPP, OverloadedStrings #-}

module System.Log.FastLogger.Date (
    DateRef
  , dateInit
  , ZonedDate
  , WebDate
  , getDate
  , getWebDate
  ) where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.IORef
import Data.UnixTime
#if WINDOWS
#define TIME UTCTime
#define GETTIME getCurrentTime
#else
import System.Posix (EpochTime, epochTime)
#define TIME EpochTime
#define GETTIME epochTime
#endif

-- | A type for zoned date.
type ZonedDate = ByteString
-- | A type for web date defined in RFC 2616.
type WebDate = ByteString

data DateCache = DateCache {
    unixTime  :: !TIME
  , zonedDate :: !ZonedDate
  , webDate   :: !WebDate
  }

-- | Reference to the 'ZonedDate' cache.
newtype DateRef = DateRef (IORef DateCache)

-- | Getting 'ZonedDate' from the cache.
getDate :: DateRef -> IO ZonedDate
getDate dref = get zonedDate dref

-- | Getting 'WebDate' from the cache.
getWebDate :: DateRef -> IO WebDate
getWebDate dref = get webDate dref

get :: (DateCache -> a) -> DateRef -> IO a
get getter (DateRef ref) = do
    newEt <- GETTIME
    cache <- readIORef ref
    let oldEt = unixTime cache
    if oldEt == newEt then
        return $ getter cache
      else do
        let newCache = newDate newEt
        writeIORef ref newCache
        return $ getter newCache

newDate :: TIME -> DateCache
newDate et = DateCache et zDate wDate
  where
    ut = fromEpochTime et
    apacheFormat = "%d/%b/%Y:%T %z"
    zDate = formatUnixTime apacheFormat ut
    wDate = formatUnixTimeGMT webDateFormat ut

-- | Initializing the 'ZonedDate' cache.
dateInit :: IO DateRef
dateInit = DateRef <$> (newDate <$> GETTIME >>= newIORef)
