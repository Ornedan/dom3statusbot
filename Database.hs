{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Database where

import Data.Time
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

import DatabaseFlags
import GameInfo


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Game
  -- Primary key, address and port
  host      String
  port      Int
  Address host port

  -- Where did we get this game from
  source    GameSource
  
  -- Any extra flags?
  flags     [GameFlag]

  -- When we last poked it
  lastPoll  UTCTime
  
  -- Data from the game itself
  lowerName String -- Lower case name
  gameInfo  GameInfo
  deriving Show
Listen
  game GameId
  nick String
  UniqueListen game nick
  deriving Show
|]
