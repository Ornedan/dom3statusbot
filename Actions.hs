{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Actions where
import System.Environment
import Prelude hiding (catch, log)

import Control.Concurrent
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
import System.IO.Error (ioeGetErrorString)
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

caughtAction :: Action a -> (String -> Action a) -> Action a
caughtAction action handler =
  action
  `catch'` (\(e :: BotException) -> do
               msg <- case e of
                 FailSilent      -> return ""
                 FailMessage msg -> return msg
               handler msg)
  `catch'` (\(e :: IOException) ->
             handler $ ioeGetErrorString e)

forkAction :: Action () -> Action ThreadId
forkAction action = do
  state <- ask
  liftIO $ forkIO $ runReaderT action state

waitAction :: Int -> Action () -> Action ()
waitAction wait action = do
  state <- ask
  liftIO $ do
    mv <- newEmptyMVar
    forkIO $ do
      runReaderT action state
      putMVar mv ()
    timeout wait $ takeMVar mv
  return ()

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
  cto <- asks (cConnectTimeout . sConfig)
  pto <- asks (cPollTimeout . sConfig)
  
  log DEBUG $ printf "Querying game %s:%d" host port
  
  mhandle <- liftIO $ timeout (cto * 1000 * 1000) $ connect
  
  when (isNothing mhandle) $ do
    failMsg $ printf "Trying to connect to %s:%d timed out" host port
  
  let handle = fromJust mhandle
  
  mgame <- liftIO $ timeout (pto * 1000 * 1000) $ do
    game <- getGame handle
    hClose handle
    return game
  
  when (isNothing mgame) $ do
    failMsg $ printf "Querying game from %s:%d timed out" host port
  
  return $ fromJust mgame
  
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
  
  log DEBUG $ printf "Updating game %s:%d" host port
  
  -- Query the game's status
  game <- requestGameInfo host port
  now <- getTime
  
  -- Update DB
  runDB $ update key [GameLastPoll  =. now,
                      GameLowerName =. (toLowercase $ name game),
                      GameGameInfo  =. game]
  
  -- Check if something worth notifying the channel about has happened
  let oldGame = gameGameInfo old
  notifications key oldGame game
  
  where
    notifications key old new
      | state old == Waiting && state new == Running = do
        notifyStart
        log INFO $ printf "Announced game start in %s." (name new)
      | turn old /= turn new                         = do
        -- Announce the new turn to channel and to listeners
        announce =<< notifyNewTurn
        notifyListens
        -- Guess stales - except when the timer is turned off.
        -- It might be possible falsely skip this if it's possible to poll the
        -- game at exactly TTH 0 and we happen to do so.
        -- But that's fairly unlikely.
        when (timeToHost old /= 0) $
          guessStales
          
        log INFO $ printf "Announced new turn in %s. (%s) -> (%s)" (name new) (show old) (show new)
      | timeToHost old == 0 && timeToHost new /= 0 = do
        notifyTimerOn
        log INFO $ printf "Announced timer on in %s." (name new)
      | timeToHost old /= 0 && timeToHost new == 0 = do
        notifyTimerOff
        log INFO $ printf "Announced timer off in %s." (name new)
      | otherwise = return ()
      where
        notifyStart = announce $ printf "Game started: %s" (name new)
        
        notifyTimerOn = announce $ printf "Timer turned on in %s" (name new)
        notifyTimerOff = announce $ printf "Timer turned off in %s" (name new)
        
        notifyNewTurn = return $ execWriter $ do
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
        
        notifyListens = do
          listens <- runDB $ selectList [ListenGame ==. key] []
          forM_ listens $ \listen -> do
            nick <- return $ listenNick $ entityVal listen
            sayTo (fromString nick) =<< notifyNewTurn
            log INFO $ printf "Notified %s of new turn in %s" nick (name new)
        
        guessStales = do
          let tthSecs    = timeToHost old `div` 1000
              stales     = filter (not . submitted) $ filter ((== Human) . player) $ nations old
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
    
    runDB $ insert $ Game server port Manual now (toLowercase $ name game) game
    
    log NOTICE $ printf "Added game %s" (name game)
    respond $ printf "Added game %s" (name game)


-- | Remove and announce the removal if the given game exists
unregister :: Action ()
unregister = do
  address <- getArgumentAddress
  ent <- runDB $ getBy address
  
  when (isJust ent) $ do
    runDB $ deleteBy address
    
    log NOTICE $ printf "Removed game %s" (name $ gameGameInfo $ entityVal $ fromJust ent)
    respond $ printf "Removed game %s" (name $ gameGameInfo $ entityVal $ fromJust ent)


-- | Respond with the given game's current status
status :: Action ()
status = do
  address <- getArgumentAddress
  ent <- runDB $ getBy address
  
  when (isJust ent) $ do
    -- Force immediate update attempt and wait up to 3s for it to complete
    waitAction (3 * 1000 * 1000) $ updateGame $ fromJust ent
    
    -- Use whatever the game status is now
    ent <- runDB $ getBy address
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
      let tth     = timeToHost game
      tell $
        printf "%s: %s, %d/%d left to submit"
        (name game)
        (showTime tth sincePoll)
        (length $ filter (not . submitted) players)
        (length $ players)
      let nAIs = length $ filter ((== AI) . player) $ nations game
      when (nAIs > 0) $
        tell $ printf " (%d AIs)" nAIs
      when (sincePoll > 5 * 60 * 1000) $
        tell $ printf ". Last poll %s ago" (formatTime sincePoll)
    
    showTime :: Int -> Int -> String
    showTime tth sincePoll
      | tth == 0  = "no timer"
      | otherwise = printf "TTH %s" $ formatTime $ tth - sincePoll
    
    formatTime :: Int -> String
    formatTime ms =
      let (hours, ms') = ms `quotRem` (60 * 60 * 1000)
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

listen :: Action ()
listen = do
  address <- getArgumentAddress
  nick <- asks (toString . fromJust . mNick . sMsg)
  
  -- Require the game exists
  gameEnt <- runDB $ getBy address
  when (isNothing gameEnt) failSilent
  let key  = entityKey $ fromJust gameEnt
      game = entityVal $ fromJust gameEnt
  
  -- Require that this would not be a duplicate entry
  listenEnt <- runDB $ selectFirst [ListenGame ==. key,
                                    ListenNick ==. nick] []
  
  -- Register the listen and notify the user
  runDB $ insert $ Listen key nick
  
  sayTo (fromString nick) $ printf "You will be sent messages about new turns in %s" (name $ gameGameInfo game)


unlisten :: Action ()
unlisten = do
  address <- getArgumentAddress
  nick <- asks (toString . fromJust . mNick . sMsg)
  
  -- Require the game exists
  gameEnt <- runDB $ getBy address
  when (isNothing gameEnt) failSilent
  let key  = entityKey $ fromJust gameEnt
      game = entityVal $ fromJust gameEnt
  
  -- Remove the listen and notify the user
  runDB $ deleteBy $ UniqueListen key nick
  
  sayTo (fromString nick) $ printf "You will no longer be sent messages about new turns in %s" (name $ gameGameInfo game)


-- | Quit if given the correct code
quit :: String -> MVar () -> Action ()
quit code quitMV = do
  args <- asks sArgs
  irc <- asks sIrc
  
  if args == fromString code
    then liftIO $ do
    disconnect irc "Terminating"
    putMVar quitMV ()
    
    else do
    nick <- asks (mNick . sMsg)
    log WARNING $ printf "Quit requested with invalid code by %s" (show nick)
