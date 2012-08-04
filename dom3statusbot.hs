{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import Prelude hiding (catch, log)

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer hiding (listen)
import Data.ByteString.Char8(ByteString(..))
import Data.ByteString.UTF8 (fromString, toString)
import Data.List ((\\), intercalate)
import Data.Maybe
import Data.Yaml
import Database.Persist.Sqlite
import Network.SimpleIRC
import Numeric (showHex)
import System.Exit
import System.IO
import System.IO.Error (ioeGetErrorString)
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (fileHandler, streamHandler)
import System.Log.Logger
import System.Random
import Text.Printf

import qualified Data.ByteString as B

import Actions
import Config
import BotException
import Database
import GameInfo
import Scheduler
import Util



pollLoop :: ActionState -> MIrc -> IO ()
pollLoop baseState irc = do
  let state    = baseState { sIrc = irc }
      interval = fromIntegral $ cPollInterval $ sConfig baseState
  
  let pollLoop' = do
        -- Poll games
        games <- runDB $ selectList [] []
        forM games $ forkAction . updateGame'
        
        -- Schedule next poll
        scheduleAction' interval pollLoop'
  
  -- Start the loop
  flip runReaderT state pollLoop'
  
  where
    updateGame' ent =
      updateGame ent
      `catch'` (\(e :: BotException) -> do
                   case e of
                     FailSilent -> return ()
                     FailMessage msg -> do
                       log WARNING $ printf "Exception in pollLoop: %s" msg)
      `catch'` (\(e :: IOException) -> do
                   log WARNING $ printf "Exception in pollLoop: %s" (ioeGetErrorString e))


mkEvent :: ActionState -> ConnectionPool -> String -> Action () -> EventFunc
mkEvent baseState pool command action = event
  where
    -- Add command string check
    action' = do
      let cmdStr = fromString $ "!" ++ command
          prefix = fromString $ "!" ++ command ++ " "
      msg <- asks (mMsg . sMsg)
      when (prefix `B.isPrefixOf` msg || msg == cmdStr) $
        action
    -- And exception handlers
    action'' = action'
               `catch'` (\(e :: BotException) -> do
                            case e of
                              FailSilent -> return ()
                              FailMessage msg -> respond msg)
               `catch'` (\(e :: IOException) -> do
                            log WARNING $ printf "Exception in %s: %s" command (ioeGetErrorString e)
                            respond "Command execution failed")
               `catch'` (\(ErrorCall msg) -> do
                            log WARNING $ printf "Exception in %s: %s" msg
                            respond "Command execution failed")

    -- Drop the leading '!', the command string itself and the following spaces
    parseMessage cmd msg = B.dropWhile (== 0x20) $ B.drop (length cmd + 1) msg
    
    -- Fill in the rest of the state fields once we're invoked
    event irc msg =
      let state = baseState { sIrc  = irc,
                              sMsg  = msg,
                              sArgs = parseMessage command (mMsg msg) }
      in runReaderT action'' state



main = withSqlitePool "bot.db" 1 $ \pool -> do
  -- Set up stderr logging
  errLog <- do
    h <- streamHandler stderr DEBUG
    return $ setFormatter h $ simpleLogFormatter "[$time : $prio] $msg"
  updateGlobalLogger rootLoggerName (setHandlers [errLog])
  updateGlobalLogger rootLoggerName (setLevel DEBUG)
  
  -- Load config
  mconfig <- decodeFile "bot.conf"
  when (isNothing mconfig) $ do
    criticalM rootLoggerName $ "Could not load configuration from bot.conf"
    exitFailure
  let Just config = mconfig
  
  -- Set up log file
  logFile <- do
    h <-fileHandler "bot.log" (read $ cLogLevel config)
    return $ setFormatter h $ simpleLogFormatter "[$time : $prio] $msg"
  updateGlobalLogger (cLogName config) (addHandler logFile)

  noticeM (cLogName config) "Bot starting up, configuration loaded OK"
  
  -- Start scheduler
  sched <- mkScheduler

  -- Set up IRC
  let state = AS { sConfig = config,
                   sPool   = pool,
                   sSched  = sched,
                   sIrc    = error "Read unitialised sIrc",
                   sMsg    = error "Read unitialised sMsg",
                   sArgs   = error "Read unitialised sArgs" }
      events = map (Privmsg . (uncurry $ mkEvent state pool))
               [("register",   register),
                ("unregister", unregister),
                ("status",     status),
                ("mods",       listMods),
                ("list",       listGames),
                ("listen",     listen),
                ("unlisten",   unlisten)]
      ircConfig' = mkDefaultConfig (cIrcServer config) (cIrcNick config) 
      ircConfig = ircConfig' { cChannels = [cIrcChannel config], 
                               cEvents   = events }
  
  -- Init DB (if necessary)
  runSqlPool (runMigration migrateAll) pool
  
  -- Connect to IRC
  eIrc <- connect ircConfig True False
  case eIrc of
    Left err -> ioError err
    Right irc -> do
      -- Set up quit command      
      code <- replicateM 4 (randomIO :: IO Int) >>= return . concatMap (flip showHex "" . abs)
      quitMV <- newEmptyMVar
      noticeM (cLogName config) $ printf "Quit code: '%s'" code
      addEvent irc $ Privmsg $ mkEvent state pool "quit" $ quit code quitMV
      
      -- Start game pollers
      forkIO $ pollLoop state irc
      
      -- Wait for quit
      takeMVar quitMV
      exitSuccess
