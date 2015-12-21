{-# LANGUAGE BangPatterns #-}

-- | Apache style logger for WAI applications.
--
-- An example:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > module Main where
-- >
-- > import Blaze.ByteString.Builder (fromByteString)
-- > import Control.Monad.IO.Class (liftIO)
-- > import qualified Data.ByteString.Char8 as BS
-- > import Network.HTTP.Types (status200)
-- > import Network.Wai (Application, responseBuilder)
-- > import Network.Wai.Handler.Warp (run)
-- > import Network.Wai.Logger (withStdoutLogger, ApacheLogger)
-- >
-- > main :: IO ()
-- > main = withStdoutLogger $ \aplogger ->
-- >     run 3000 $ logApp aplogger
-- >
-- > logApp :: ApacheLogger -> Application
-- > logApp aplogger req response = do
-- >     liftIO $ aplogger req status (Just len)
-- >     response $ responseBuilder status hdr msg
-- >   where
-- >     status = status200
-- >     hdr = [("Content-Type", "text/plain")]
-- >     pong = "PONG"
-- >     msg = fromByteString pong
-- >     len = fromIntegral $ BS.length pong

module Network.Wai.Logger (
  -- * High level functions
    ApacheLogger
  , withStdoutLogger
  -- * Creating a logger
  , ApacheLoggerActions(..)
  , initLogger
  -- * Types
  , IPAddrSource(..)
  , LogType(..)
  , FileLogSpec(..)
  -- * Date cacher
  , clockDateCacher
  , ZonedDate
  , DateCacheGetter
  , DateCacheUpdater
  -- * Utilities
  , logCheck
  , showSockAddr
  ) where

import Control.AutoUpdate (mkAutoUpdate, defaultUpdateSettings, updateAction)
import Control.Concurrent (MVar, newMVar, tryTakeMVar, putMVar)
import Control.Exception (handle, SomeException(..), bracket)
import Control.Monad (when, void)
import Network.HTTP.Types (Status)
import Network.Wai (Request)
import System.EasyFile (getFileSize)
import System.Log.FastLogger

import Network.Wai.Logger.Apache
import Network.Wai.Logger.Date
import Network.Wai.Logger.IORef
import Network.Wai.Logger.IP (showSockAddr)

----------------------------------------------------------------

-- | Executing a function which takes 'ApacheLogger'.
--   This 'ApacheLogger' writes log message to stdout.
--   Each buffer (4K bytes) is flushed every second.
withStdoutLogger :: (ApacheLogger -> IO a) -> IO a
withStdoutLogger app = bracket setup teardown $ \(aplogger, _) ->
    app aplogger
  where
    setup = do
        (getter, _updater) <- clockDateCacher
        apf <- initLogger FromFallback (LogStdout 4096) getter
        let aplogger = apacheLogger apf
            remover = logRemover apf
        return (aplogger, remover)
    teardown (_, remover) = void remover

----------------------------------------------------------------

-- | Apache style logger.
type ApacheLogger = Request -> Status -> Maybe Integer -> IO ()

data ApacheLoggerActions = ApacheLoggerActions {
    -- | Essentially, this is equivalent to @\\req st mlen -> apacheLog req st mlen >>= pushLog@.
    apacheLogger :: ApacheLogger
    -- | Generating Apache log format. Use this if you don't want leak 'Request'.
  , apacheLog :: Request -> Status -> Maybe Integer -> IO LogStr
    -- | Pushing the apache log.
  , pushLog :: LogStr -> IO ()
    -- | Removing resources relating Apache logger.
    --   E.g. flushing and deallocating internal buffers.
  , logRemover :: IO ()
    -- | This is obsoleted. Rotation is done on-demand.
    --   So, this is now an empty action.
  , logRotator :: IO ()
  }

-- | Logger Type.
data LogType = LogNone                     -- ^ No logging.
             | LogStdout BufSize           -- ^ Logging to stdout.
                                           --   'BufSize' is a buffer size
                                           --   for each capability.
             | LogFile FileLogSpec BufSize -- ^ Logging to a file.
                                           --   'BufSize' is a buffer size
                                           --   for each capability.
                                           --   File rotation is done on-demand.
             | LogCallback (LogStr -> IO ()) (IO ())

----------------------------------------------------------------

-- |
-- Creating 'ApacheLogger' according to 'LogType'.
initLogger :: IPAddrSource -> LogType -> DateCacheGetter
           -> IO ApacheLoggerActions
initLogger _     LogNone             _       = noLoggerInit
initLogger ipsrc (LogStdout size)    dateget = stdoutLoggerInit ipsrc size dateget
initLogger ipsrc (LogFile spec size) dateget = fileLoggerInit ipsrc spec size dateget
initLogger ipsrc (LogCallback cb flush) dateget = callbackLoggerInit ipsrc cb flush dateget

----------------------------------------------------------------

noLoggerInit :: IO ApacheLoggerActions
noLoggerInit = return ApacheLoggerActions {
    apacheLogger = noLogger
  , apacheLog = \_ _ _ -> return mempty
  , pushLog = \_ -> return ()
  , logRotator = noRotator
  , logRemover = noRemover
  }
  where
    noLogger _ _ _ = return ()
    noRotator = return ()
    noRemover = return ()

stdoutLoggerInit :: IPAddrSource -> BufSize -> DateCacheGetter
                 -> IO ApacheLoggerActions
stdoutLoggerInit ipsrc size dateget = do
    lgrset <- newStdoutLoggerSet size
    let formatter = apache ipsrc dateget
        push = pushLogStr lgrset
        noRotator = return ()
        remover = rmLoggerSet lgrset
    return ApacheLoggerActions {
        apacheLogger = \req st mlen -> formatter req st mlen >>= push
      , apacheLog    = formatter
      , pushLog      = push
      , logRotator   = noRotator
      , logRemover   = remover
      }

fileLoggerInit :: IPAddrSource -> FileLogSpec -> BufSize -> DateCacheGetter
               -> IO ApacheLoggerActions
fileLoggerInit ipsrc spec size dateget = do
    lgrset <- newFileLoggerSet size $ log_file spec
    ref <- newIORef (0 :: Int)
    mvar <- newMVar ()
    let formatter = apache ipsrc dateget
        push logstr = do
            cnt <- decrease ref
            pushLogStr lgrset logstr
            when (cnt <= 0) $ tryRotate lgrset spec ref mvar
        noRotator = return ()
        remover = rmLoggerSet lgrset
    return ApacheLoggerActions {
        apacheLogger = \req st mlen -> formatter req st mlen >>= push
      , apacheLog    = formatter
      , pushLog      = push
      , logRotator   = noRotator
      , logRemover   = remover
      }

decrease :: IORef Int -> IO Int
decrease ref = atomicModifyIORef' ref (\x -> (x - 1, x - 1))

callbackLoggerInit :: IPAddrSource -> (LogStr -> IO ()) -> IO () -> DateCacheGetter
                   -> IO ApacheLoggerActions
callbackLoggerInit ipsrc cb flush dateget = do
    flush' <- mkAutoUpdate defaultUpdateSettings
        { updateAction = flush
        }
    let formatter = apache ipsrc dateget
        push = \str -> cb str >> flush'
        noRotator = return ()
        remover = return ()
    return ApacheLoggerActions {
        apacheLogger = \req st mlen -> formatter req st mlen >>= push
      , apacheLog    = formatter
      , pushLog      = push
      , logRotator   = noRotator
      , logRemover   = remover
      }

----------------------------------------------------------------

apache :: IPAddrSource -> DateCacheGetter -> Request -> Status -> Maybe Integer -> IO LogStr
apache !ipsrc !dateget !req !st !mlen = do
    !zdata <- dateget
    return $! apacheLogStr ipsrc zdata req st mlen

----------------------------------------------------------------

tryRotate :: LoggerSet -> FileLogSpec -> IORef Int -> MVar () -> IO ()
tryRotate lgrset spec ref mvar = bracket lock unlock rotateFiles
  where
    lock           = tryTakeMVar mvar
    unlock Nothing = return ()
    unlock _       = putMVar mvar ()
    rotateFiles Nothing = return ()
    rotateFiles _       = do
        msiz <- getSize
        case msiz of
            -- A file is not available.
            -- So, let's set a big value to the counter so that
            -- this function is not called frequently.
            Nothing -> writeIORef ref 1000000
            Just siz
                | siz > limit -> do
                    rotate spec
                    renewLoggerSet lgrset
                    writeIORef ref $ estimate limit
                | otherwise -> do
                    writeIORef ref $ estimate (limit - siz)
    file = log_file spec
    limit = log_file_size spec
    getSize = handle (\(SomeException _) -> return Nothing) $ do
        -- The log file is locked by GHC.
        -- We need to get its file size by the way not using locks.
        Just . fromIntegral <$> getFileSize file
    -- 200 is an ad-hoc value for the length of log line.
    estimate x = fromInteger (x `div` 200)

----------------------------------------------------------------

-- |
-- Checking if a log file can be written if 'LogType' is 'LogFile'.
logCheck :: LogType -> IO ()
logCheck LogNone          = return ()
logCheck (LogStdout _)    = return ()
logCheck (LogFile spec _) = check spec
logCheck (LogCallback _ _) = return ()
