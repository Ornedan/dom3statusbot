{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Actions where
import System.Environment
import Prelude hiding (catch, log)

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Trans.Writer hiding (liftCatch)
import Data.ByteString.Char8(ByteString(..))
import Data.ByteString.UTF8 (fromString, toString)
import Data.List ((\\), intercalate)
import Data.Maybe
import Data.Time
import Database.Persist
import Database.Persist.Sql
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
import DatabaseFlags
import GameInfo
import Protocol
import ThreadPool
import Util


data ActionState = AS { sConfig :: Config,
                        sCPool  :: ConnectionPool,
                        sTPool  :: ThreadPool,
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

--runDB :: SqlPersistT IO a -> Action a
runDB act = do
  pool <- asks sCPool
  liftIO $ runResourceT $ runStderrLoggingT $ runSqlPool act pool

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
{-
forkAction :: Action () -> Action ThreadId
forkAction action = do
  state <- ask
  liftIO $ forkIO $ runReaderT action state
-}
forkAction :: Action () -> Action ()
forkAction action = do
  state <- ask
  
  liftIO $ runInPool (sTPool state) $ runReaderT action state
  --liftIO $ forkIO $ runReaderT action state

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

delay :: Int -> Action ()
delay secs = liftIO $ longThreadDelay $ 10^6 * fromIntegral secs


requestGameInfo :: String -> Int -> Action GameInfo
requestGameInfo host port = requestGameInfo' host port
                            `catch'`
                             (\(e :: BotException) -> throw e)
                            `catch'`
                            (\(e :: SomeException) -> failMsg $ printf "requestGameInfo %s:%d failed with exception: %s" host port (show e))

requestGameInfo' host port = do
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
  
  let game = fromJust mgame
  
  log DEBUG $ printf "Got response for game %s:%d: %s turn %d, TTH %s" host port (name game) (turn game) (msecToString $ timeToHost game)
  
  return game
  
  where
    connect = connectTo host (PortNumber $ fromIntegral port)
              `catch` (\(e :: IOException) -> do
                          failMsg $ printf "Could not connect to %s:%d" host port)

requestCurrentGameInfo :: (String, Int) -> Action (Maybe (Entity Game))
requestCurrentGameInfo (host, port) = do
  ment <- runDB $ getBy $ Address host port
  
  case ment of
    Nothing -> return Nothing
    Just ent  -> do
      -- Force immediate update attempt and wait up to 3s for it to complete
      waitAction (3 * 1000 * 1000) $ updateGame ent
      
      -- Return whatever is now the game status
      runDB $ getBy $ Address host port

getArguments :: Action [String]
getArguments = asks sArgs >>= return . map toString . filter (not . B.null) . B.split 0x20

getArgumentAddress :: [String] -> Action ((String, Int), [String])
getArgumentAddress args = do
  case args of
    -- No arguments were present? Fail the operation
    [] -> failSilent
    -- One argument is assumed to be the name of a game
    [name] -> do
      maddr <- loadAddress name
      when (isNothing maddr)
        failSilent
      return (fromJust maddr, [])
    -- Two or more: name or host and port, then flags
    (arg1:arg2:rest) -> do
      -- Is the first one the name of a game?
      maddr <- loadAddress arg1
      case maddr of
        -- It is
        Just addr -> return (addr, arg2:rest)
        -- Nope. Assume the first two are host and port
        Nothing -> do
          let host  = toLowercase arg1 -- Normalise addresses to lowercase
              port' = arg2
          port <- liftIO $
                  catch (readIO port') $
                  (\(e :: SomeException) -> failMsg "Invalid host & port. Note that Dominions 3 does not allow game names to contain spaces.")
          return ((host, port), rest)

  where
    loadAddress = undefined
    {-
    loadAddress name' = do
      let name = toLowercase name'
      ment <- runDB $ selectFirst [GameLowerName ==. name] []
      case ment of
        Nothing  -> return Nothing
        Just ent -> let game = entityVal ent 
                    in return $ Just (gameHost game, gamePort game) -}

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
  pollInterval <- asks (cPollInterval . sConfig)
  notifications (NoAnnounce `notElem` gameFlags old) pollInterval (gameLastPoll old) now key oldGame game
  
  where
    notifications canAnnounce pollInterval lastPoll curPoll key old new
      | state old == Waiting && state new == Running = do
        notifyStart
        log INFO $ printf "Announced game start in %s." (name new)
      | turn old /= turn new                         = do
        -- Announce the new turn to channel and to listeners
        when canAnnounce (announce =<< makeNewTurnMessage)
        notifyListens
        -- Guess stales - except when the timer is turned off.
        -- It might be possible falsely skip this if it's possible to poll the
        -- game at exactly TTH 0 and we happen to do so.
        -- But that's fairly unlikely.
        when (tthOld /= 0) $
          guessStales
        
        log INFO $ printf "Announced new turn in %s. (%s) -> (%s)" (name new) (show old) (show new)
      --- Timer changes
      | tthOld == 0 && tthNew /= 0 = do
        notifyTimerOn
        log INFO $ printf "Announced timer on in %s." (name new)
      | tthOld /= 0 && tthNew == 0 = do
        notifyTimerOff
        log INFO $ printf "Announced timer off in %s." (name new)
      | let tthDelta = tthNew - tthOld
            elapsed = floor $ (curPoll `diffUTCTime` lastPoll) * 1000
        in tthDelta > 0 ||                                       -- Timer value has grown
           abs tthDelta > elapsed + 5 * pollInterval * 1000 = do -- Or timer has changed noticeably more than the elapsed time
        notifyTimerChange
        log INFO $ printf "Announced timer change in %s: %s at %s -> %s at %s." (name new) (msecToString tthOld) (show lastPoll) (msecToString tthNew) (show curPoll)
      | otherwise = return ()
      where
        tthOld = timeToHost old
        tthNew = timeToHost new
        notifyStart =
          when canAnnounce $ announce $ printf "Game started: %s" (name new)
        notifyTimerOn =
          when canAnnounce $ announce $ printf "Timer turned on in %s, now %s" (name new) (msecToString tthNew)
        notifyTimerOff =
          when canAnnounce $ announce $ printf "Timer turned off in %s" (name new)
        notifyTimerChange =
          when canAnnounce $ announce $ printf "Timer changed in %s, now %s" (name new) (msecToString tthNew)
        
        makeNewTurnMessage = return $ execWriter $ do
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
            sayTo (fromString nick) =<< makeNewTurnMessage
            log INFO $ printf "Notified %s of new turn in %s" nick (name new)
        
        guessStales = do
          let tthSecs    = tthOld `div` 1000
              stales     = filter (not . submitted) $ filter ((== Human) . player) $ nations old
              present    = filter connected stales
              notPresent = filter (not . connected) stales
          -- It's not staling if the turn changes well enough before the deadline
          pollInterval <- asks (cPollInterval . sConfig)
          when (canAnnounce && tthSecs < 3 * pollInterval && length stales > 0) $ announce $ execWriter $ do
            tell $ printf "Potential stales, estimating from %ds before hosting" tthSecs
            when (length notPresent > 0) $ do
              tell ". Not submitted: "
              tell $ intercalate ", " $ map (nationName . nationId) notPresent
            when (length present > 0) $ do
              tell ". Connected, but not submitted: "
              tell $ intercalate ", " $ map (nationName . nationId) present


-- | Add the game to tracked games if it's not there yet
register :: Action ()
register = do
  ((server, port), rest) <- getArguments >>= getArgumentAddress
  flags <- forM rest $ \flag ->
    liftIO $ catch (readIO flag) (\(e :: SomeException) -> failMsg "Invalid flag")
  
  -- Check that no such game is registered yet
  ent <- runDB $ getBy $ Address server port
  when (isNothing ent) $ do
    -- Query game, add it to DB and respond affirmatively
    now <- getTime
    game <- requestGameInfo server port
    
    runDB $ insert $ Game server port Manual flags now (toLowercase $ name game) game
    
    log NOTICE $ printf "Added game %s" (name game)
    respond $ printf "Added game %s" (name game)


-- | Remove and announce the removal if the given game exists
unregister :: Action ()
unregister = do
  (host, port) <- getArguments >>= getArgumentAddress >>= return . fst
  ment <- runDB $ getBy $ Address host port
  
  when (isJust ment) $ do
    let ent = fromJust ment
    
    runDB $ do
      deleteWhere [ListenGame ==. entityKey ent]
      deleteBy $ Address host port
    
    log NOTICE $ printf "Removed game %s" (name $ gameGameInfo $ entityVal ent)
    respond $ printf "Removed game %s" (name $ gameGameInfo $ entityVal ent)


-- | Respond with the given game's current status
status :: Action ()
status = do
  address <- getArguments >>= getArgumentAddress >>= return . fst
  ment <- requestCurrentGameInfo address
  
  when (isJust ment) $ do
    now <- getTime
    let game      = entityVal $ fromJust ment
        info      = gameGameInfo game
        sincePoll = floor $ 1000 * diffUTCTime now (gameLastPoll game)
    respond $ showGame sincePoll info
    
  where
    showGame sincePoll game
      | state game == Waiting = showWaiting game
      | otherwise             = showRunning sincePoll game
    showWaiting game =
      printf "%s: %s; Waiting for players, %d pretenders submitted"
      (name game)
      (maybe "Unknown era" showEra $ era game)
      (length $ filter ((== Human) . player) $ nations game)
    showRunning sincePoll game = execWriter $ do
      let players      = filter ((== Human) . player) $ nations game
      let tth          = timeToHost game
      let notSubmitted = filter (not . submitted) players
      tell $
        printf "%s: turn %d, %s, %d/%d left to submit"
        (name game)
        (turn game)
        (showTime tth sincePoll)
        (length notSubmitted)
        (length $ players)
      when (length notSubmitted > 0 && length notSubmitted <= 3) $ do
        tell ": "
        tell $ intercalate ", " $ map (nationName . nationId) notSubmitted
      let nAIs = length $ filter ((== AI) . player) $ nations game
      when (nAIs > 0) $
        tell $ printf ", %d AIs" nAIs
      when (sincePoll > 5 * 60 * 1000) $
        tell $ printf ". Last poll %s ago" (msecToString sincePoll)
    
    showEra :: Era -> String
    showEra Early  = "EA"
    showEra Middle = "MA"
    showEra Late   = "LA"
    
    showTime :: Int -> Int -> String
    showTime tth sincePoll
      | tth == 0  = "no timer"
      | otherwise = printf "TTH %s" $ msecToString $ tth - sincePoll


-- | Respond with the given game's current detailed status
details :: Action ()
details = do
  -- Set the response to always go to the
  state <- ask
  let state' = state { sMsg = (sMsg state) { mOrigin = mNick $ sMsg $ state } }
  liftIO $ runReaderT details' state'
  
  where
    details' = do
      -- First, normal status
      status
      
      -- Then per-nation info
      (host, port) <- getArguments >>= getArgumentAddress >>= return . fst
      ment <- runDB $ getBy $ Address host port
      
      when (isJust ment) $ do
        let game = entityVal $ fromJust ment
            info = gameGameInfo game
        
        -- Show host and port
        respond $ printf " Address: %s:%d" host port
        
        forM_ (nations info) $ respond . showNation info
    
    showNation :: GameInfo -> Nation -> String
    showNation game nation = execWriter $ do
      -- Basic info
      tell $ printf "  %s: %s" (nationName $ nationId nation) (showPlayer $ player nation)
      
      -- For human players, show submitted & connected.
      -- Only relevant when the game is running
      when (state game == Running && player nation == Human) $ do
        tell ", "
        tell $ if submitted nation then "submitted" else "not submitted"
        when (connected nation) $ do
          tell $ ", connected"
    
    showPlayer :: Player -> String
    showPlayer Empty            = "empty slot"
    showPlayer Human            = "human"
    showPlayer AI               = "AI"
    showPlayer Closed           = "closed slot"
    showPlayer DefeatedThisTurn = "defeated this turn"
    showPlayer DefeatedEarlier  = "defeated"


-- | Show the list of mods used in the given game
listMods :: Action ()
listMods = do
  (host, port) <- getArguments >>= getArgumentAddress >>= return . fst
  ent <- runDB $ getBy $ Address host port
  
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
  (host, port) <- getArguments >>= getArgumentAddress >>= return . fst
  nick <- asks (toString . fromJust . mNick . sMsg)
  
  -- Require the game exists
  gameEnt <- runDB $ getBy $ Address host port
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
  (host, port) <- getArguments >>= getArgumentAddress >>= return . fst
  nick <- asks (toString . fromJust . mNick . sMsg)
  
  -- Require the game exists
  gameEnt <- runDB $ getBy $ Address host port
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

