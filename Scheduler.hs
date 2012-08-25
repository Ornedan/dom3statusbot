module Scheduler where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Data.Map (Map(..))
import Data.Maybe
import Data.Time

import qualified Data.Map as Map

import ThreadManager
import Util


data Scheduler = Scheduler { manager :: ThreadManager,
                             lock    :: MVar (), 
                             tasks   :: MVar (Map (UTCTime, Int) (IO ())), 
                             waitTId :: MVar (Maybe ThreadId),
                             counter :: MVar Int }


locked :: Scheduler -> IO a -> IO a
locked scheduler action = modifyMVar (lock scheduler) $ \() -> do
  v <- action
  return $! ((), v)


mkScheduler :: ThreadManager -> IO Scheduler
mkScheduler tm = do
  lock <- newMVar ()
  tasks <- newMVar $ Map.empty
  waitTId <- newMVar Nothing
  counter <- newMVar 0
  
  return $! Scheduler { manager = tm,
                        lock    = lock, 
                        tasks   = tasks,
                        waitTId = waitTId,
                        counter = counter }


schedule :: Scheduler -> UTCTime -> IO () -> IO ()
schedule scheduler when task = locked scheduler $ do
  -- Store the new task
  n <- modifyMVar (counter scheduler) $ \n -> return (n + 1, n)
  modifyMVar_ (tasks scheduler) $ return . Map.insert (when, n) task
  
  -- Kill the old wake-up thread and spawn a new one
  modifyMVar_ (waitTId scheduler) $ \tid -> do
    maybe (return ()) killThread tid
    tid' <- fork (manager scheduler) $ run scheduler
    return $ Just tid'

schedule' :: Scheduler -> NominalDiffTime -> IO () -> IO ()
schedule' scheduler offset task = do
  now <- getCurrentTime
  schedule scheduler (offset `addUTCTime` now) task


run :: Scheduler -> IO ()
run scheduler = do
  -- Pick the earliest task
  ((time, _), task) <- locked scheduler $ withMVar (tasks scheduler) $ return . Map.findMin
  
  -- Wait until it's time
  now <- getCurrentTime
  let wait = ceiling $ 10^6 * (toRational $ time `diffUTCTime` now)
  longThreadDelay wait

  -- Remove the task from the queue and run it
  locked scheduler $ do
    modifyMVar_ (tasks scheduler) $ return . Map.deleteMin
    void $ fork (manager scheduler) task

  -- Schedule next task
  moreTasks <- locked scheduler $ withMVar (tasks scheduler) $ return . not . Map.null
  when (moreTasks) $ run scheduler
