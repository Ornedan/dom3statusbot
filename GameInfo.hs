module GameInfo where

import Control.DeepSeq
import Data.Map (Map)


data GameInfo = GameInfo { name       :: String,
                           state      :: GameState,
                           turn       :: Int,
                           timeToHost :: Int,
                           era        :: Maybe Era,
                           nations    :: [Nation], 
                           mods       :: [ModInfo] }
              deriving (Eq, Read, Show)

instance NFData GameInfo where
  rnf game = name       game `seq`
             state      game `seq`
             turn       game `seq`
             timeToHost game `seq`
             era        game `seq`
             nations    game `seq`
             mods       game `seq` ()

data GameState = Waiting
               | Running
               deriving(Eq, Read, Show)

data Era = Early
         | Middle
         | Late
         deriving (Eq, Read, Show)

data Nation = Nation { nationId  :: !Int,
                       player    :: !Player,
                       submitted :: !Bool,
                       connected :: !Bool }
            deriving (Eq, Read, Show)

data Player = Empty
            | Human
            | AI
            | Closed
            | DefeatedThisTurn
            | DefeatedEarlier
            deriving (Eq, Read, Show)


data ModInfo = ModInfo { modName         :: !String,
                         modMajorVersion :: !Int,
                         modMinorVersion :: !Int }
             deriving (Eq, Show, Read)


nationName :: Int -> String
nationName 0  = "EA Arcoscephale"
nationName 1  = "EA Ermor"
nationName 2  = "EA Ulm"
nationName 3  = "EA Marverni"
nationName 4  = "EA Sauromatia"
nationName 5  = "EA T'ien Ch'i"
nationName 6  = "EA Machaka"
nationName 7  = "EA Mictlan"
nationName 8  = "EA Abysia"
nationName 9  = "EA Caelum"
nationName 10 = "EA C'tis"
nationName 11 = "EA Pangaea"
nationName 12 = "EA Agartha"
nationName 13 = "EA Tir na n'Og"
nationName 14 = "EA Fomoria"
nationName 15 = "EA Vanheim"
nationName 16 = "EA Helheim"
nationName 17 = "EA Niefelheim"
nationName 18 = "EA Kailasa"
nationName 19 = "EA Yomi"
nationName 20 = "EA Hinnom"
nationName 21 = "EA Atlantis"
nationName 22 = "EA R'lyeh"
nationName 26 = "EA Oceania"
nationName 68 = "EA Lanka"

nationName 27 = "MA Arcoscephale"
nationName 28 = "MA Ermor"
nationName 29 = "MA Pythium"
nationName 30 = "MA Man"
nationName 31 = "MA Ulm"
nationName 32 = "MA Marignon"
nationName 33 = "MA Mictlan"
nationName 34 = "MA T'ien Ch'i"
nationName 35 = "MA Machaka"
nationName 36 = "MA Agartha"
nationName 37 = "MA Abysia"
nationName 38 = "MA Caelum"
nationName 39 = "MA C'tis"
nationName 40 = "MA Pangaea"
nationName 41 = "MA Vanheim"
nationName 42 = "MA Jotunheim"
nationName 43 = "MA Bandar Log"
nationName 44 = "MA Shinuyama"
nationName 45 = "MA Ashdod"
nationName 46 = "MA Atlantis"
nationName 47 = "MA R'lyeh"
nationName 48 = "MA Oceania"
nationName 69 = "MA Eriu"

nationName 49 = "LA Arcoscephale"
nationName 50 = "LA Ermor"
nationName 51 = "LA Man"
nationName 52 = "LA Ulm"
nationName 53 = "LA Marignon"
nationName 54 = "LA Mictlan"
nationName 55 = "LA T'ien Ch'i"
nationName 56 = "LA Jomon"
nationName 57 = "LA Agartha"
nationName 58 = "LA Abysia"
nationName 59 = "LA Caelum"
nationName 60 = "LA C'tis"
nationName 61 = "LA Pangaea"
nationName 62 = "LA Midgård"
nationName 63 = "LA Utgård"
nationName 64 = "LA Patala"
nationName 65 = "LA Gath"
nationName 66 = "LA Atlantis"
nationName 67 = "LA R'lyeh"
nationName 70 = "LA Pythium"
nationName 71 = "LA Bogarus"

nationName 23 = "AI Special monsters 1"
nationName 24 = "AI Special monsters 2"
nationName 25 = "AI Independents"

nationName n  = "Nation " ++ show n