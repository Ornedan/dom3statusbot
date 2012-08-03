{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Actions where

import Prelude hiding (catch, log)

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer hiding (liftCatch)
import Data.ByteString.Char8(ByteString(..))
import Data.ByteString.UTF8 (fromString, toString)
import Data.List ((\\), intercalate)
import Data.Maybe
import Data.Time
import Database.Persist
import Database.Persist.GenericSql
import Network
import Network.SimpleIRC
import System.Exit
import System.Log.Logger
import System.IO
import System.Timeout
import Text.Printf

import qualified Data.ByteString as B

import BotException
import Config
import Database
import GameInfo
import Protocol
import Scheduler
import Util


data ActionState = AS { sConfig :: Config,
                        sPool   :: ConnectionPool,
                        sSched  :: Scheduler,
                        sIrc    :: MIrc,
                        sMsg    :: IrcMessage, 
                        sArgs   :: ByteString }

type Action a = ReaderT ActionState IO a


respond :: String -> Action ()
respond str = do
  sayer <- asks (mOrigin . sMsg)
  when (isJust sayer) $
    sayTo (fromJust sayer) str

announce :: String -> Action ()
announce str = do
  chan <- asks (cIrcChannel . sConfig)
  sayTo (fromString chan) str

sayTo :: ByteString -> String -> Action ()
sayTo to str = do
  irc <- asks sIrc
  liftIO $ sendMsg irc to $ fromString str

runDB :: SqlPersist IO a -> Action a
runDB act = do
  pool <- asks sPool
  liftIO $ runSqlPool act pool

getTime :: Action UTCTime
getTime = liftIO getCurrentTime

log :: Priority -> String -> Action ()
log prio str = do
  logName <- asks (cLogName . sConfig)
  liftIO $ logM logName prio str

catch' :: Exception e => Action a -> (e -> Action a) -> Action a
catch' = liftCatch catch

scheduleAction :: UTCTime -> Action () -> Action ()
scheduleAction when action = do
  state <- ask
  liftIO $ schedule (sSched state) when $ runReaderT action state

scheduleAction' :: NominalDiffTime -> Action () -> Action ()
scheduleAction' offset action = do
  state <- ask
  liftIO $ schedule' (sSched state) offset $ runReaderT action state


requestGameInfo :: String -> Int -> Action GameInfo
requestGameInfo host port = do
  secs <- asks (cConnectTimeout . sConfig)
  log INFO $ printf "Querying game %s:%d" host port
  
  mhandle <- liftIO $ timeout (secs * 1000 * 1000) $ connect
  
  case mhandle of
    Nothing -> do
      failMsg $ printf "Trying to connect to %s:%d timed out" host port
    Just handle -> liftIO $ do
      game <- getGame handle
      hClose handle
      return game
  
  where
    connect = connectTo host (PortNumber $ fromIntegral port)
              `catch` (\(e :: IOException) -> do
                          failMsg $ printf "Could not connect to %s:%d" host port)


getArgumentAddress :: Action (Unique (GameGeneric a) b)
getArgumentAddress = do
  args <- asks sArgs
  case B.break (== 0x20) args of
    -- No arguments were present? Do nothing
    ("", "")    -> failSilent
    -- One argument is assumed to be the name of a game
    (name', "") -> do
      let name = toLowercase $ toString name'
      mgame <- runDB $ selectFirst [GameLowerName ==. name] []
      when (isNothing mgame) failSilent
      let game = entityVal $ fromJust mgame
      return $ Address (gameHost game) (gamePort game)
    -- Two arguments: host, port
    (host, port') -> do
      port <- liftIO $
              catch (readIO $ toString port') $
              (\(e :: SomeException) -> failMsg "Invalid port")
      return $ Address (toString host) port


-- | Poll the given game and update DB accordingly.
updateGame :: Entity Game -> Action ()
updateGame oldEnt = do
  let key  = entityKey oldEnt
      old  = entityVal oldEnt
      host = gameHost old
      port = gamePort old
  
  log INFO $ printf "Updating game %s:%d" host port
  
  -- Query the game's status
  game <- requestGameInfo host port
  now <- getTime
  
  -- Update DB
  runDB $ update key [GameLastPoll  =. now,
                      GameLowerName =. (toLowercase $ name game),
                      GameGameInfo  =. game]
  
  -- Check if something worth notifying the channel about has happened
  notifications (gameGameInfo old) game  
  
  where
    notifications old new
      | state old == Waiting && state new == Running = notifyStart
      | turn old /= turn new                         = notifyNewTurn >> guessStales
      | otherwise = return ()
      where
        notifyStart = announce $ printf "Game started: %s" (name new)
        notifyNewTurn = announce $ execWriter $ do
          tell $ printf "New turn in %s (%d)" (name new) (turn new)
          
          let defeated = filter ((== DefeatedThisTurn) . player) $ nations new
          when (length defeated > 0) $ do
            tell ". Defeated: "
            tell $ intercalate ", " $ map (nationName . nationId) defeated
          
          let oldAIs = filter ((== AI) . player) $ nations old
              newAIs = filter ((== AI) . player) $ nations new
              goneAI = newAIs \\ oldAIs
          when (length goneAI > 0) $ do
            tell ". Gone AI: "
            tell $ intercalate ", " $ map (nationName . nationId) goneAI
        guessStales = do
          let tthSecs    = timeToHost old `div` 1000
              stales     = filter (not . submitted) $ filter ((== Human) . player) $ nations new
              present    = filter connected stales
              notPresent = filter (not . connected) stales
          -- It's not staling if the turn changes well enough before the deadline
          pollInterval <- asks (cPollInterval . sConfig)
          when (tthSecs < 3 * pollInterval && length stales > 0) $ announce $ execWriter $ do
            tell $ printf "Potential stales, estimating from %ds before hosting" tthSecs
            when (length notPresent > 0) $ do
              tell ". Not submitted: "
              tell $ intercalate ", " $ map (nationName . nationId) notPresent
            when (length present > 0) $ do
              tell ". Connected, but no submitted: "
              tell $ intercalate ", " $ map (nationName . nationId) present


-- | Add the game to tracked games if it's not there yet
register :: Action ()
register = do
  address@(Address server port) <- getArgumentAddress
  
  -- Check that no such game is registered yet
  ent <- runDB $ getBy address
  when (isNothing ent) $ do
    -- Query game, add it to DB and respond affirmatively
    now <- getTime
    game <- requestGameInfo server port
    
    runDB $ insert $ Game server port now (toLowercase $ name game) game
    
    respond $ printf "Added game %s" (name game)


-- | Remove and announce the removal if the given game exists
unregister :: Action ()
unregister = do
  address <- getArgumentAddress
  ent <- runDB $ getBy address
  
  when(isJust ent) $ do
    runDB $ deleteBy address
    respond $ printf "Removed game %s" (name $ gameGameInfo $ entityVal $ fromJust ent)


-- | Respond with the given game's current status
status :: Action ()
status = do
  address <- getArgumentAddress
  ent <- runDB $ getBy address
  
  when (isJust ent) $ do
    now <- getTime
    let game      = entityVal $ fromJust ent
        info      = gameGameInfo game
        sincePoll = floor $ 1000 * diffUTCTime now (gameLastPoll game)
    respond $ showGame sincePoll info
    
  where
    showGame sincePoll game
      | state game == Waiting = showWaiting game
      | otherwise             = showRunning sincePoll game
    showWaiting game =
      printf "%s: Waiting for players, %d pretenders submitted"
      (name game)
      (length $ filter ((== Human) . player) $ nations game)
    showRunning sincePoll game = execWriter $ do
      let players = filter ((== Human) . player) $ nations game
      tell $
        printf "%s: TTH %s, %d/%d left to submit"
        (name game)
        (formatTime sincePoll $ timeToHost game)
        (length $ filter (not . submitted) players)
        (length $ players)
      let nAIs = length $ filter ((== AI) . player) $ nations game
      when (nAIs > 0) $
        tell $ printf " (%d AIs)" nAIs
      
    
    formatTime :: Int -> Int -> String
    formatTime sincePoll tth =
      let ms           = if tth == 0 then 0 else tth - sincePoll
          (hours, ms') = ms `quotRem` (60 * 60 * 1000)
          (mins, ms'') = ms' `quotRem` (60 * 1000)
          secs         = ms'' `quot` 1000
      in printf "%02d:%02d:%02d" hours mins secs


-- | Show the list of mods used in the given game
listMods :: Action ()
listMods = do
  address <- getArgumentAddress
  ent <- runDB $ getBy address
  
  when (isJust ent) $ do
    let game = gameGameInfo $ entityVal $ fromJust ent
    respond $ printf "Mods used in %s: %s" (name game) (showMods game)
  
  where
    showMods game
      | mods game == [] = "none"
      | otherwise       = intercalate ", " $ map modName $ mods game


-- | Show the list of games being currently tracked
listGames :: Action ()
listGames = do
  games <- runDB $ selectList [] [Asc GameLowerName]
  let names = map (name . gameGameInfo . entityVal) games
  
  respond $ printf "Tracking games: %s" (intercalate ", " names)


-- | Quit if given the correct code
quit :: String -> MVar () -> Action ()
quit code quitMV = do
  args <- asks sArgs
  irc <- asks sIrc
  
  when (args == fromString code) $ liftIO $ do
    disconnect irc "Terminating"
    putMVar quitMV ()