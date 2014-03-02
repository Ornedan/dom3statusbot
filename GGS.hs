{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}

module GGS 
       (ggsLoop)
       where

import Prelude hiding (log)

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import Data.Aeson
import Data.ByteString.Lazy (ByteString(..))
import Data.Maybe
import Data.Set (Set)
import Database.Persist.Sqlite
import Network.HTTP
import Network.SimpleIRC
import Network.URI
import System.Log.Logger
import Text.Printf

import qualified Data.Set as Set

import Actions
import Config
import BotException
import Database
import DatabaseFlags
import GameInfo
import Util


instance FromJSON (String, Int) where  
  parseJSON (Object v) = (,) <$>
                         v .: "server" <*>
                         v .: "port"
  parseJSON _          = mzero


request :: Request ByteString
request = replaceHeader HdrAccept "application/json, text/javascript, */*; q=0.01" $
          replaceHeader HdrAcceptEncoding "gzip, deflate" $
          replaceHeader (HdrCustom "X-Requested-With") "XMLHttpRequest" $
          mkRequest GET $ fromJust $ parseURI "http://www.brainwrinkle.net/"


pollGGS :: IO [(String, Int)]
pollGGS = do
  resp <- simpleHTTP request >>= getResponseBody

  case decode resp of
    Nothing    -> failMsg $ "Failed to decode games from: " ++ (show resp)
    Just games -> return $ map (toLowercase *** id) games


ggsLoop :: ActionState -> MIrc -> IO ()
ggsLoop baseState irc = do
  let state    = baseState { sIrc = irc }
      interval = fromIntegral $ cGGSPollInterval $ sConfig baseState
  
  forever $ flip runReaderT state $ do
    log INFO $ printf "Polling GGS's games list"
    -- Get currently known games from DB and GGS
    dbGames <- runDB $ selectList [] []
    mggsGames <- pollGGS'
    
    when (isJust mggsGames) $ do
      let ggsGames = fromJust mggsGames
          dbSet   = Set.fromList $ map ((gameHost &&& gamePort) . entityVal) dbGames
          ggsSet  = Set.fromList ggsGames
          -- Games known by GGS and not by us
          added   = Set.toList $ ggsSet Set.\\ dbSet
          -- Games known by us, but not by GGS. Note that these might also just be games
          -- not registered on GGS.
          removed = Set.toList $ dbSet Set.\\ ggsSet
      
      forM_ added add
      forM_ removed remove
    
    delay interval
  
  where
    pollGGS' = liftIO (pollGGS >>= return . Just)
               `caughtAction`
               (\msg -> do
                   when (not $ null msg) $ log WARNING $ printf "pollGGS: Polling failed: %s"  msg
                   return Nothing)
               
    add (host, port) =
      add' host port
      `caughtAction`
      (\msg -> when (not $ null msg) $ log WARNING $ printf "pollGGS: Failed to add game (%s:%d): %s" host port msg)
    add' host port = do
      ent <- runDB $ getBy (Address host port)
      when (isNothing ent) $ do
        now <- getTime
        game <- requestGameInfo host port
        
        runDB $ insert $ Game host port GGS [] now (toLowercase $ name game) game
        
        let msg = printf "Added game %s from GGS (%s:%d)" (name game) host port
        log NOTICE msg
        announce msg
    
    remove (host, port) =
      remove' host port
      `caughtAction`
      (\msg -> when (not $ null msg) $ log WARNING $ printf "pollGGS: Failed to remove game (%s:%d): %s" host port msg)
    remove' host port = do
      let address = Address host port
      ment <- runDB $ getBy address
      
      -- Only remove games that were added from GGS here. Anything added
      -- manually needs to be removed manually.
      when (isJust ment) $ do
        let ent = fromJust ment
        when (gameSource (entityVal ent) == GGS) $ do
          runDB $ do
            deleteWhere [ListenGame ==. entityKey ent]
            deleteBy address
          
          let msg = printf "Removed game %s (%s:%d)" (name $ gameGameInfo $ entityVal ent) host port
          log NOTICE msg
          announce msg
