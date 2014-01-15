{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module DatabaseFlags where

import Database.Persist.TH


data GameSource = Manual
                | GGS
                deriving (Eq, Read, Show)

data GameFlag = NoAnnounce
              deriving (Eq, Read, Show)
                

derivePersistField "GameSource"
derivePersistField "GameFlag"
