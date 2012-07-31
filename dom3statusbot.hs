{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import Prelude hiding (catch, log)

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
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
import Util



pollLoop :: ActionState -> MIrc -> MVar () -> IO ()
pollLoop baseState irc quitMV =
  let state = baseState { sIrc = irc }
  in forever $ flip runReaderT state $ do
    -- Wait and check whether we've been ordered to shut down
    secs <- asks (cPollInterval . sConfig)
    liftIO $ do
      threadDelay $ secs * 1000 * 1000
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
                     FailMessage msg -> do
                       log WARNING $ printf "Exception in pollLoop: %s" msg)
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
        notifyNewTurn = announce $ execWriter $ do
          tell $ printf "New turn in %s (%d)" (name new) (turn new)
          
          let defeated = filter ((== DefeatedThisTurn) . player) $ nations new
          when (length defeated > 0) $ do
            tell ". Defeated: "
            tell $ intercalate ", " $ map (nationName . nationId) $ nations new
          
          let oldAIs = filter ((== AI) . player) $ nations old
              newAIs = filter ((== AI) . player) $ nations new
              goneAI = newAIs \\ oldAIs
          when (length goneAI > 0) $ do
            tell ". Gone AI: "
            tell $ intercalate ", " $ map (nationName . nationId) goneAI


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
    h <-fileHandler "bot.log" NOTICE
    return $ setFormatter h $ simpleLogFormatter "[$time : $prio] $msg"
  updateGlobalLogger (cLogName config) (addHandler logFile)

  noticeM (cLogName config) "Bot starting up, configuration loaded OK"
  
  -- Set up IRC
  let state = AS { sConfig = config,
                   sPool   = pool,
                   sIrc    = error "Read unitialised sIrc",
                   sMsg    = error "Read unitialised sMsg",
                   sArgs   = error "Read unitialised sArgs" }
      events = map (Privmsg . (uncurry $ mkEvent state pool))
               [("register",   register),
                ("unregister", unregister),
                ("status",     status),
                ("mods",       listMods),
                ("list",       listGames)]
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
      
      -- Start game polling thread
      pollLoop state irc quitMV
