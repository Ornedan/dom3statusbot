{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, ScopedTypeVariables #-}
{-# LANGUAGE GADTs, FlexibleContexts, Rank2Types #-}

module Database where

import Data.Time
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

import GameInfo


derivePersistField "GameInfo"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Game
  -- Primary key, address and port
  host      String
  port      Int
  Address host port
  
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
