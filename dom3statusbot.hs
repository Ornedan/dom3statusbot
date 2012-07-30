{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, ScopedTypeVariables #-}
{-# LANGUAGE GADTs, FlexibleContexts #-}

import Prelude hiding (catch, log)

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import Data.ByteString.Char8(ByteString(..))
import Data.ByteString.UTF8 (fromString, toString)
import Data.List (intercalate)
import Data.Maybe
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
import BotException
import Database
import GameInfo
import Util


-- Constants
ircServer  = "irc.synirc.net"
ircChannel = "#dom3goons"
ircNick    = "Treebot"

logName = "dom3statusbot"
connectTimeout = 2500 * 1000 -- 2.5s in microseconds
pollInterval = 60 * 1000 * 1000 -- 1 min in microseconds



pollLoop :: ActionState -> MIrc -> MVar () -> IO ()
pollLoop baseState irc quitMV =
  let state = baseState { sIrc = irc }
  in forever $ flip runReaderT state $ do
    -- Wait and check whether we've been ordered to shut down
    liftIO $ do
      threadDelay pollInterval
      quit <- tryTakeMVar quitMV
      when (isJust quit) exitSuccess
    
    -- Poll games
    games <- runDB $ selectList [] []
    forM games updateGame
  
  where
    updateGame ent =
      updateGame' ent
      `catch'` (\(e :: BotException) -> do
                   case e of
                     FailSilent -> return ()
                     FailMessage msg -> respond msg)
      `catch'` (\(e :: IOException) -> do
                   log WARNING $ printf "Exception in pollLoop: %s" (ioeGetErrorString e))
    updateGame' ent = do
      let key = entityKey ent
          old = entityVal ent
      
      log INFO $ printf "Updating game %s:%d" (gameHost old) (gamePort old)
      
      -- Query the game's status
      now <- getTime
      game <- requestGameInfo (gameHost old) (gamePort old)
  
      -- Update DB
      runDB $ update key [GameLastPoll  =. now,
                          GameLowerName =. (toLowercase $ name game),
                          GameGameInfo  =. game]
      
      -- Check if something worth notifying the channel about has happened
      notifications (gameGameInfo old) game
    
    notifications old new
      | state old == Waiting && state new == Running = notifyStart
      | turn old /= turn new                         = notifyNewTurn
      | otherwise = return ()
      where
        notifyStart = announce $ printf "Game started: %s" (name new)
        notifyNewTurn = announce $ printf "New turn in %s (%d)" (name new) (turn new)


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
  -- Set up logging
  errLog <- do
    h <- streamHandler stderr DEBUG
    return $ setFormatter h $ simpleLogFormatter "[$time : $prio] $msg"
  updateGlobalLogger rootLoggerName (setHandlers [errLog])
  updateGlobalLogger rootLoggerName (setLevel NOTICE)
  
  logFile <- do
    h <-fileHandler "bot.log" DEBUG
    return $ setFormatter h $ simpleLogFormatter "[$time : $prio] $msg"
  updateGlobalLogger logName (addHandler logFile)

  
  -- Set up IRC
  let state = AS { sConfig = Config { cChannel        = fromString ircChannel,
                                      cConnectTimeout = connectTimeout,
                                      cLogName        = logName },
                   sPool   = pool,
                   sIrc    = undefined,
                   sMsg    = undefined,
                   sArgs   = undefined }
      events = map (Privmsg . (uncurry $ mkEvent state pool))
               [("register",   register),
                ("unregister", unregister),
                ("status",     status),
                ("mods",       listMods),
                ("list",       listGames)]
      ircConfig = (mkDefaultConfig ircServer ircNick) { cChannels = [ircChannel], 
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
      logM logName NOTICE $ printf "Quit code: '%s'" code
      addEvent irc $ Privmsg $ mkEvent state pool "quit" $ quit code quitMV
      
      -- Start game polling thread
      pollLoop state irc quitMV
