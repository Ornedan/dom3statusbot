{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, ScopedTypeVariables #-}
{-# LANGUAGE GADTs, FlexibleContexts #-}

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Data.ByteString.Char8(ByteString(..))
import Data.ByteString.UTF8 (fromString, toString)
import Data.Char
import Data.List (intercalate)
import Data.Maybe
import Data.Time
import Database.Persist
import Database.Persist.Sqlite
import Network
import Network.SimpleIRC
import System.IO
import System.IO.Error (ioeGetErrorString)
import System.Timeout
import Text.Printf

import qualified Data.ByteString as B
import qualified Data.List as L

import Prelude hiding (catch)

import Database
import GameInfo
import Protocol
import Util


-- Constants
ircServer  = "irc.synirc.net"
ircChannel = "#dom3goons"
ircNick    = "Treebot"

pollInterval = 60 * 1000 * 1000



dispatch :: ConnectionPool -> EventFunc
dispatch pool irc msg
  -- Everything requires origin for reply target or otherwise
  | isNothing $ mOrigin msg = return ()
  | otherwise =
    let target = fromJust $ mOrigin msg
    in dispatch' pool irc msg
       -- Let's not leak exceptions into the IRC bot code
       -- No response sent in case of empty error message
       `catches` [Handler (\(e :: IOException) ->
                            case ioeGetErrorString e of
                              ""  -> return ()
                              msg -> sendMsg irc target $ fromString $ "Error: " ++ msg
                          ),
                  Handler (\(ErrorCall msg) ->
                            case msg of
                              ""  -> return ()
                              msg -> sendMsg irc target $ fromString $ "Error: " ++ msg
                          )]
  
--dispatch' :: Connection -> EventFunc
dispatch' pool irc msg
  -- Which command is it? Parse arguments and strip the space following the command
  | "!register "   `B.isPrefixOf` args = parseGame "!register "   $ register pool
  | "!unregister " `B.isPrefixOf` args = parseGame "!unregister " $ unregister pool
  | "!status "     `B.isPrefixOf` args = parseGame "!status "     $ status pool
  | "!mods "       `B.isPrefixOf` args = parseGame "!mods "       $ listMods pool
  | "!list"        `B.isPrefixOf` args = listGames pool irc msg
  | "!quit <^>v"   `B.isPrefixOf` args = disconnect irc "Terminating"
  -- Ignore everything else
  | otherwise                          = return ()

  where
    args = mMsg msg
    parseGame cmd action = do
      let raw = B.dropWhile (== 0x20) $ B.drop (B.length cmd) args
      case B.break (== 0x20) raw of
        -- No arguments, nothing to do. Responding with an error message to this would just be spam
        ("", "") -> return ()
        
        -- Got one param, assume it's name of a game. Check DB to make sure it exists.
        (name', "") -> do
          let name = toLowercase $ toString name'
          mgame <- runSqlPool (selectFirst [GameLowerName ==. name] []) pool
          when (isNothing mgame) $
            fail $ printf "Unknown game: %s" name
          let game = entityVal $ fromJust mgame
          action (Address (gameHost game) (gamePort game)) irc msg
        
        -- Got two params, assume they are host and port
        (host', port') -> do
          host <- return $ toString host'
          port <- handle (\(e :: SomeException) -> fail "Invalid port") $
                  readIO $ toString port'
          action (Address host port) irc msg

register pool address@(Address server port) irc msg = do
  -- Nothing to do if the game is already being tracked
  ent <- runSqlPool (getBy address) pool
  when (isNothing ent) $ do
    let replyTo = fromJust $ mOrigin msg
    -- Get initial game info
    now <- getCurrentTime
    game <- doRequest server port
  
    -- Store it in DB
    runSqlPool (insert $ Game server port now (toLowercase $ name game) game) pool
  
    -- Let the user know
    sendMsg irc replyTo $ fromString $ printf "Added game %s" (name game)
  

unregister pool address irc msg = do
  let replyTo = fromJust $ mOrigin msg
  
  -- We only respond if such a game exists
  ent <- runSqlPool (getBy address) pool
  when (isJust ent) $ do
    runSqlPool (deleteBy address) pool
    sendMsg irc replyTo $ fromString $ printf "Removed game %s" (name $ gameGameInfo $ entityVal $ fromJust ent)


status pool address irc msg = do
  let replyTo = fromJust $ mOrigin msg
  ent <- runSqlPool (getBy address) pool
  
  when (isJust ent) $ do
    now <- getCurrentTime
    let game      = entityVal $ fromJust ent
        info      = gameGameInfo game
        sincePoll = 1000 * (floor $ diffUTCTime now (gameLastPoll game))
    sendMsg irc replyTo $ fromString $ showGame sincePoll info
  
  where
    showGame sincePoll game
      | state game == Waiting = printf "%s: Waiting for players, %d pretenders submitted"
                                (name game)
                                (length $ filter ((== Human) . player) $ nations game)
      | otherwise             = printf "%s: TTH %s, %d left to submit"
                                (name game)
                                (formatTime sincePoll $ timeToHost game)
                                (length $
                                 filter (not . submitted) $ 
                                 filter (not . (== AI) . player) $
                                 nations game)

    formatTime :: Int -> Int -> String
    formatTime sincePoll tth =
      let ms           = if tth == 0 then 0 else tth - sincePoll
          (hours, ms') = ms `quotRem` (60 * 60 * 1000)
          (mins, ms'') = ms' `quotRem` (60 * 1000)
          secs         = ms'' `quot` 1000
      in printf "%02d:%02d:%02d" hours mins secs


listMods pool address irc msg = do
  let replyTo = fromJust $ mOrigin msg
  ent <- runSqlPool (getBy address) pool
  
  when (isJust ent) $ do
    let game = gameGameInfo $ entityVal $ fromJust ent
    sendMsg irc replyTo $ fromString $ printf "Mods used in %s: %s" (name game) (showMods game)
  
  where
    showMods game
      | mods game == [] = "none"
      | otherwise       = intercalate ", " $ map modName $ mods game


listGames pool irc msg = do
  let replyTo = fromJust $ mOrigin msg
  games <- runSqlPool (selectList [] []) pool
  let names = map (name . gameGameInfo . entityVal) games
  
  sendMsg irc replyTo $ fromString $ printf "Tracking games: %s" (intercalate ", " names)


doRequest :: String -> Int -> IO GameInfo
doRequest address port = do
  mh <- timeout (2500 * 1000) $ connectTo address (PortNumber $ fromIntegral port)
  maybe
    (fail $ printf "Trying to connect to %s:%s timed out" address (show port))
    (\h -> do game <- getGame h
              hClose h
              return game)
    mh


pollLoop :: ConnectionPool -> MIrc -> IO ()
pollLoop pool irc = forever $ do
  threadDelay pollInterval
  games <- runSqlPool (selectList [GamePort >. 0] []) pool
  forM games updateGame
  
  where
    updateGame ent = updateGame' ent `catches`
                     -- Ignore game fetch failures. TODO: Log if I add logging
                     [Handler $ \(e :: IOException) -> putStrLn $ "Error: " ++ ioeGetErrorString e]
    updateGame' ent = do
      let key = entityKey ent
          old = entityVal ent
      
      printf "Updating game %s:%d\n" (gameHost old) (gamePort old)
      
      -- Query the game's status
      now <- getCurrentTime
      game <- doRequest (gameHost old) (gamePort old)
  
      -- Update DB
      runSqlPool
        (update key [GameLastPoll  =. now,
                     GameLowerName =. (toLowercase $ name game),
                     GameGameInfo  =. game])
        pool
      
      -- Check if something worth notifying the channel about has happened
      notifications (gameGameInfo old) game
    
    notifications old new
      | state old == Waiting && state new == Running = notifyStart
      | turn old /= turn new                         = notifyNewTurn
      | otherwise = return ()
      where
        notifyStart =
          sendMsg irc (fromString ircChannel) $ fromString $ printf "Game started: %s" (name new)
        notifyNewTurn =
          sendMsg irc (fromString ircChannel) $ fromString $ printf "New turn in %s (%d)" (name new) (turn new)

main = withSqlitePool "bot.db" 1 $ \pool -> do
  let baseConfig = mkDefaultConfig ircServer ircNick
      ircConfig = baseConfig { cChannels = [ircChannel], 
                               cEvents = [Privmsg $ dispatch pool] }
  
  -- Init DB (if necessary)
  runSqlPool (runMigration migrateAll) pool
  
  -- Connect to IRC
  eIrc <- connect ircConfig True True
  case eIrc of
    Left err -> ioError err
    Right irc -> do
      -- Start game polling thread
      pollLoop pool irc
