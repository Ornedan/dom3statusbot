module Util where

import Control.Concurrent
import Data.Char
import Data.Int
import Data.List
import Text.Printf



msecToString :: Int -> String
msecToString msec =
  let ms           = abs msec
      (hours, ms') = ms `quotRem` (60 * 60 * 1000)
      (mins, ms'') = ms' `quotRem` (60 * 1000)
      secs         = ms'' `quot` 1000
  in printf "%s%02d:%02d:%02d" (if msec < 0 then "-" else "" :: String) hours mins secs


toLowercase :: String -> String
toLowercase = map toLower


-- | Pause the thread for the given number of microseconds.  There is no guarantee that the thread will be restarted promptly
-- after the delay, but it will not be started before then.
-- 
-- Similar to "threadDelay", but takes a 64-bit argument.  The Haskell 2010 specification says that (maxBound :: Int) is at least 
-- 2^29-1.  However 2^29 microseconds is only about 538 seconds.  GHC on a 32-bit machine has a 32 bit Int, but that is still less
-- than 36 minutes.  64-bit signed integers give a maximum delay of over 292 million years, which should be sufficient.
-- 
-- Snaffled from timers-updateable by Paolo Veronelli.
longThreadDelay :: Int64 -> IO ()
longThreadDelay d = mapM_ (threadDelay . fromIntegral) $ unfoldr f d
   where
      f d1 | d1 <= 0     = Nothing
           | d1 < maxInt = Just (d1, 0)
           | otherwise   = Just (maxInt, d1-maxInt)
      maxInt = fromIntegral (maxBound :: Int)  -- Platform-dependent