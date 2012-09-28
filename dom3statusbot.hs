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
import GHC.Conc
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
import GGS
import ThreadPool
import Util



pollLoop :: ActionState -> MIrc -> IO ()
pollLoop baseState irc = do
  let state    = baseState { sIrc = irc }
      interval = fromIntegral $ cPollInterval $ sConfig baseState
  
  forever $ flip runReaderT state $ do
    -- Poll games
    games <- runDB $ selectList [] []
    forM_ games $ forkAction . updateGame'
    
    -- Sleep until next poll
    delay interval
  
  where
    updateGame' ent =
      updateGame ent
      `caughtAction`
      (\msg -> when (not $ null msg) $ log WARNING $ printf "Exception in pollLoop: %s" msg)


mkEvent :: ActionState -> ConnectionPool -> String -> Action () -> EventFunc
mkEvent baseState pool command action = event
  where
    -- Add command string check
    action' = do
      let cmdStr = fromString $ "!" ++ command
          prefix = fromString $ "!" ++ command ++ " "
      msg <- asks (mMsg . sMsg)
      origin <- asks (mOrigin . sMsg)
      when (prefix `B.isPrefixOf` msg || msg == cmdStr) $ do
        log INFO $
          printf "Action '%s' requested by '%s': %s"
          command (maybe "-" toString origin) (toString msg)
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


mkEvents :: ActionState -> ConnectionPool -> [(Action (), String, String)] -> [IrcEvent]
mkEvents baseState pool events = mkHelp : map (\(action, command, _) -> Privmsg $ mkEvent baseState pool command action) events
  where
    mkHelp = Privmsg $ mkEvent baseState pool "help" $ do
      let longest = maximum $ 4 : map (\(_,cmd,_) -> length cmd) events
          pattern = printf "!%%-%ds %%s" longest
      respond $ printf pattern ("help" :: String) ("Display this list of commands." :: String)
      respond $ printf "Commands which take a game as an argument may be given the name of a game, or the address and port. Most commands only work if the game is being tracked."
      forM_ events $ \(_, command, description) -> do
        respond $ printf pattern command description


main = withSqlitePool "bot.db" 1 $ \connPool -> do
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
  
  -- Set up thread pool
  threadPool <- mkThreadPool 4 $ cLogName config
  
  -- Set up IRC
  let state = AS { sConfig = config,
                   sCPool  = connPool,
                   sTPool  = threadPool,
                   sIrc    = error "Read unitialised sIrc",
                   sMsg    = error "Read unitialised sMsg",
                   sArgs   = error "Read unitialised sArgs" }
      events = mkEvents state connPool
               [(register,   "register",
                 "Add a game to be tracked by the bot. Takes two arguments: address and port."),
                (unregister, "unregister",
                 "Stop the bot tracking the given game."),
                (status,     "status",
                 "Show information about the current status of the given game."),
                (details,    "details",
                 "Show details information about the current status of the given game."),
                (listMods,   "mods",
                 "Show the names of the mods used in the given game."),
                (listGames,  "list",
                 "List the names of the games being tracked by the bot."),
                (listen,     "listen",
                 "Set yourself to be notified of new turns in the given game. Note that the messages will be sent to the nick you used when !listening."),
                (unlisten,   "unlisten",
                 "Remove yourself from the list of people notified of turns in the given game.")]
      ircConfig' = mkDefaultConfig (cIrcServer config) (cIrcNick config) 
      ircConfig = ircConfig' { cChannels = [cIrcChannel config], 
                               cEvents   = events }
  
  -- Init DB (if necessary)
  runSqlPool (runMigration migrateAll) connPool
  
  -- Connect to IRC
  eIrc <- connect ircConfig True False
  case eIrc of
    Left err -> ioError err
    Right irc -> do
      -- Set up quit command      
      code <- replicateM 4 (randomIO :: IO Int) >>= return . concatMap (flip showHex "" . abs)
      quitMV <- newEmptyMVar
      noticeM (cLogName config) $ printf "Quit code: '%s'" code
      addEvent irc $ Privmsg $ mkEvent state connPool "quit" $ quit code quitMV
      
      -- Start game pollers
      forkIO (pollLoop' state irc) >>= flip labelThread "pollLoop-start"
      
      -- Start GGS polling
      forkIO (ggsLoop' state irc) >>= flip labelThread "ggsLoop-start"

      -- Wait for quit
      takeMVar quitMV
      exitSuccess

  where
    pollLoop' state irc =
      pollLoop state irc
      `catch`
      (\(e :: SomeException) -> criticalM (cLogName $ sConfig state) $ "pollLoop crashed, exception: " ++ show e)
    ggsLoop' state irc =
      ggsLoop state irc
      `catch`
      (\(e :: SomeException) -> criticalM (cLogName $ sConfig state) $ "ggsLoop crashed, exception: " ++ show e)
