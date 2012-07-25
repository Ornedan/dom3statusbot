module Protocol where

import Control.DeepSeq (deepseq)
import Control.Monad (replicateM, when)
import Data.Binary.Get
import Data.ByteString.Lazy.UTF8 (toString)
import Data.Maybe (isJust, fromJust)
import Data.Word (Word8)
import System.IO
import Text.Printf

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import qualified Data.Map as Map

import GameInfo



-- | Number of nation slots.
numberOfNations = 95


-- | Parse the response to a status request.
parseStatus :: B.ByteString -> GameInfo
parseStatus body = runGet parseStatus' $ BL.fromChunks [body]
  where
    parseStatus' = do
      -- Message type code
      0x04 <- getWord8
      
      -- Unknown stuff, seems constant
      0x23 <- getWord8
      0x01 <- getWord8
      0x00 <- getWord8
      0x00 <- getWord8
      
      -- The interesting fields begin
      gameState <- parseGameState
      name <- getLazyByteStringNul >>= return . toString
      era <- parseEra
      
      -- Another constant(?) bit
      0x2d <- getWord8
      
      -- Time to host, in milliseconds
      time <- getWord32le
      
      -- Some unknown field
      0x00 <- getWord8
      
      -- Player slot fields
      players <- replicateM numberOfNations getWord8
      submitteds <- replicateM numberOfNations getWord8
      connecteds <- replicateM numberOfNations getWord8
      
      -- Turn number
      turn <- parseTurn
      
      -- Some unknown field
      skipOneOf [0x00, 0x01]
      
      -- There should be no more input left
      rem <- remaining
      when (rem /= 0) $
        fail $ printf "parseStatus: %d bytes remain unread" rem
      
      return GameInfo { name       = name,
                        state      = gameState,
                        turn       = turn,
                        timeToHost = fromIntegral time,
                        era        = era,
                        nations    = map fromJust $ filter isJust $
                                     map (uncurry parseNation) $
                                     zip [0 ..] $ zip3 players submitteds connecteds,
                        mods       = undefined }
    
    parseGameState = do
      byte <- getWord8
      case byte of
        0x01 -> return Waiting
        0x02 -> return Running
        _    -> fail $ printf "parseGameState: Unrecognized value %d" byte
    
    parseEra = do
      byte <- getWord8
      case byte of
        0x00 -> return Nothing
        0x01 -> return $ Just Early
        0x02 -> return $ Just Middle
        0x03 -> return $ Just Late
        _    -> fail $ printf "parseEra: Unrecognized value %d" byte

    parseTurn = do
      n <- getWord32le
      if n == 0xffffffff
        then return 0
        else return $ fromIntegral n

    parsePlayer 0x00 = Empty
    parsePlayer 0x01 = Human
    parsePlayer 0x02 = AI
    parsePlayer 0xfd = Closed
    parsePlayer 0xfe = DefeatedThisTurn
    parsePlayer 0xff = DefeatedEarlier
    parsePlayer byte = error $ printf "parsePlayer: Unrecognized value %d" byte

    parseSubmitted 0x00 = False
    parseSubmitted 0x01 = True
    
    parseConnected 0x00 = False
    parseConnected 0x01 = True

    parseNation nth (0x00, 0x00, 0x00) = Nothing -- Empty slot
    parseNation 25  (0x03, 0x00, 0x00) = Nothing -- Independents special slot
    parseNation nth (player, submitted, connected) =
      Just Nation { nationId  = nth, 
                    player    = parsePlayer player, 
                    submitted = parseSubmitted submitted,
                    connected = parseConnected connected }
                               

-- | Parse the response to a mod list request.
parseMods :: B.ByteString -> [ModInfo]
parseMods body = runGet parseMods' $ BL.fromChunks [body]
  where
    parseMods' = do
      0x12 <- getWord8 -- Message type code
      num <- getWord16le >>= return . fromIntegral
      if num == 0xffff
        then return []
        else replicateM (num + 1) parseMod
    
    parseMod = do
      major <- getWord16le >>= return . fromIntegral
      minor <- getWord16le >>= return . fromIntegral
      name <- getLazyByteStringNul >>= return . toString
      
      return $ ModInfo { modName = name,
                         modMajorVersion = major,
                         modMinorVersion = minor }


-- | Make a request message with given type code.
mkRequest :: Word8 -> B.ByteString
mkRequest typeCode = B.pack [0x66, 0x48, 0x01, 0x00, 0x00, 0x00, typeCode]

requestStatus = mkRequest 0x03

requestMods = mkRequest 0x11

requestBye = mkRequest 0x0b


-- | Skip a byte if it's value is in the given list, otherwise error.
skipOneOf :: [Word8] -> Get ()
skipOneOf bs = do
  byte <- getWord8
  when (not $ byte `elem` bs) $
    fail $ printf "skipOneOf: got %d, expected one of %s" byte (show bs)

-- | Write to handle and flush.
write :: Handle -> B.ByteString -> IO ()
write h q = do
  B.hPut h q
  hFlush h

-- | Send the given message and return the response body, beginning after the body
--   length field.
doMessage :: Handle -> B.ByteString -> IO B.ByteString
doMessage h m = do
  write h m
  
  header <- B.hGetSome h 6
  let bodyLength = flip runGet (BL.fromChunks [header]) $ do
        0x66 <- getWord8 -- f
        0x48 <- getWord8 -- H in all the message types we use
        getWord32le >>= return . fromIntegral
  
  body <- B.hGetSome h bodyLength
  when (B.length body /= bodyLength) $
    fail $ printf "Length mismatch: header field %d; actual body %d" bodyLength (B.length body)

  return body


-- | Communicate with a Dominions 3 server via the given handle. Requests game status and mods
--   listings and sends bye.
--   Returns the game info received.
getGame :: Handle -> IO GameInfo
getGame handle = do
  game <- doMessage handle requestStatus >>= return . parseStatus
  mods <- doMessage handle requestMods >>= return . parseMods
  
  doMessage handle requestBye
  
  let game' = game { mods = mods }
  
  deepseq game' $ return game'
