{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, ScopedTypeVariables #-}
{-# LANGUAGE GADTs, FlexibleContexts, Rank2Types #-}

module Database where

import Data.Time
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

import GameInfo


derivePersistField "GameInfo"


data GameSource = Manual
                | GGS
                deriving (Eq, Read, Show)

derivePersistField "GameSource"


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Game
  -- Primary key, address and port
  host      String
  port      Int
  Address host port

  -- Where did we get this game from
  source    GameSource

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
