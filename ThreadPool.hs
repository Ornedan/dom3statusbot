{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module ThreadPool 
       (ThreadPool, mkThreadPool, runInPool)
       where

import Prelude hiding (catch)

import GHC.Conc
import Control.Concurrent.Chan
import Control.Exception
import Control.Monad
import System.Log.Logger



data ThreadPool = ThreadPool { chan    :: Chan (IO ()),
                               threads :: [ThreadId] }


mkThreadPool :: Int -> String -> IO ThreadPool
mkThreadPool n logName = do
  chan <- newChan
  threads <- replicateM n $ forkIO $ poolThread chan logName
  forM (zip threads [1 ..]) $ \(tid, n) -> labelThread tid $ "poolThread-" ++ (show n)
  
  return ThreadPool { chan = chan,
                      threads = threads }

runInPool :: ThreadPool -> IO () -> IO ()
runInPool = writeChan . chan

poolThread :: Chan (IO ()) -> String -> IO ()
poolThread chan logName = forever $ do
  action <- readChan chan
  action `catch` logException
  
  where
    logException :: SomeException -> IO ()
    logException e = logM logName WARNING $ "Uncaught exception in poolThread: " ++ show e

{-







import Control.Concurrent      (ThreadId, forkIO)
import Control.Concurrent.MVar (MVar, modifyMVar, newEmptyMVar, newMVar, putMVar, takeMVar, tryTakeMVar, readMVar)
import Control.Exception       (SomeException, try)
import Control.Monad           (join, replicateM, when)
import qualified Data.Map as M

data ThreadStatus =
    Running
  | Finished
  | Threw SomeException
  deriving Show

newtype ThreadManager = TM (MVar (M.Map ThreadId (MVar ThreadStatus)))
  deriving Eq

-- | Make a thread manager.
make :: IO ThreadManager
make = TM `fmap` newMVar M.empty

-- | Make a managed thread. Uses 'forkIO'.
fork :: ThreadManager -> IO () -> IO ThreadId
fork (TM tm) action =
    modifyMVar tm $ \m -> do
        state <- newEmptyMVar
        tid <- forkIO $ do
            r <- try action
            putMVar state (either Threw (const Finished) r)
        return (M.insert tid state m, tid)

-- | Make the given number of managed threads.
forkn :: ThreadManager -> Int -> IO () -> IO [ThreadId]
forkn tm n = replicateM n . fork tm

-- | Get the status of a managed thread.
getStatus :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
getStatus (TM tm) tid =
    modifyMVar tm $ \m ->
      case M.lookup tid m of
        Nothing    -> return (m, Nothing)
        Just state -> tryTakeMVar state >>= \mst ->
          return $
            case mst of
              Nothing  -> (m, Just Running)
              Just sth -> (M.delete tid m, Just sth)

-- | Block until a specific managed thread terminates.
waitFor :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
waitFor (TM tm) tid =
    join . modifyMVar tm $ \m ->
      return $
        case M.updateLookupWithKey (\_ _ -> Nothing) tid m of
          (Nothing, _)     -> (m, return Nothing)
          (Just state, m') -> (m', Just `fmap` takeMVar state)

-- | Block until all managed threads terminate.
waitForAll :: ThreadManager -> IO ()
waitForAll tm@(TM tmMvar) = do
    threadMap <- readMVar tmMvar
    let threads = M.keys threadMap
    statuses <- mapM (getStatus tm) threads
    _ <- mapM (waitFor tm) threads
    Control.Monad.when (foldr checkStatus False statuses) $
        waitForAll tm
  where
    checkStatus :: Maybe ThreadStatus -> Bool -> Bool
    checkStatus _ True = True
    checkStatus (Just Running) False = True
    checkStatus _ False = False
-}