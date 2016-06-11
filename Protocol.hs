{-# LANGUAGE OverloadedStrings #-}
module Protocol where

import Codec.Compression.Zlib
import Control.DeepSeq (deepseq)
import Control.Monad (replicateM, when)
import Data.Binary.Get (Get(..),
                        getByteString,
                        getLazyByteStringNul,
                        getWord8,
                        getWord16le,
                        getWord32le,
                        remaining,
                        runGet,
                        skip)
import Data.ByteString.Lazy.Builder
import Data.ByteString.Lazy.Builder.ASCII
import Data.ByteString.Char8(ByteString(..))
import Data.ByteString.Lazy.UTF8 (toString)
import Data.Maybe (isJust, fromJust)
import Data.Word (Word8)
import Network
import System.IO
import Text.Printf

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import qualified Data.Map as Map

import GameInfo


-- | Number of nation slots.
numberOfNations = 200


-- | Parse the response to a status request.
parseStatus :: B.ByteString -> GameInfo
parseStatus body = runGet parseStatus' $ BL.fromStrict body
  where
    parseStatus' = do
      -- Message type
      require $ B.pack [0x04]
      
      -- Unknown stuff, probably a word32le but not sure
      skip 4 -- ?? 01 00 00
      
      -- The interesting fields begin
      gameState <- parseGameState
      name <- getLazyByteStringNul >>= return . toString
      era <- parseEra

      -- Unknown word32
      getWord32le
      
      -- More unknown stuff, just before the TTH
      require "-"
      
      -- Time to host, in milliseconds
      time <- getWord32le
      
      -- Some unknown stuff
      require $ B.pack [0x00]
      
      -- Player slot fields
      players <- replicateM numberOfNations getWord8
      submitteds <- replicateM numberOfNations getWord8
      connecteds <- replicateM numberOfNations getWord8
      
      -- Turn number
      turn <- parseTurn
      
      -- Some unknown field
      requireOneOf [0x00, 0x01]
      skip 3
      require $ B.pack [0x00]
      
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
                                     map (uncurry $ parseNation gameState) $
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

    parseSubmitted 0x00 = None
    parseSubmitted 0x01 = Partial
    parseSubmitted 0x02 = Full
    
    parseConnected 0x00 = False
    parseConnected 0x01 = True

    parseNation _ nth (0x00, 0x00, 0x00) = Nothing -- Empty slot
    parseNation _ nth (0x03, 0x00, 0x00) = Nothing -- Independents special slot
    parseNation Waiting nth (player, submitted, connected) =
      Just Nation { nationId  = nth,
                    player    = parsePlayer player,
                    submitted = None,
                    connected = parseConnected connected }
    parseNation Running nth (player, submitted, connected) =
      Just Nation { nationId  = nth, 
                    player    = parsePlayer player, 
                    submitted = parseSubmitted submitted,
                    connected = parseConnected connected }

-- | Parse the response to a mod list request.
parseMods :: B.ByteString -> [ModInfo]
parseMods body = runGet parseMods' $ BL.fromStrict body
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
      skip 4 -- No idea what this is
      
      return $ ModInfo { modName = name,
                         modMajorVersion = major,
                         modMinorVersion = minor }


-- | Make a request message with given type code.
mkRequest :: Word8 -> B.ByteString
mkRequest typeCode = B.pack [0x66, 0x48, 0x01, 0x00, 0x00, 0x00, typeCode]

requestStatus = mkRequest 0x03

requestMods = mkRequest 0x11

requestBye = mkRequest 0x0b

-- | Require the next bytes of input be the same as the given bytestring.
require :: B.ByteString -> Get ()
require str = do
  got <- getByteString $ B.length str
  when (not $ got == str) $
    fail $ printf "require: got %s, expected %s" (show got) (show str)

-- | Require the next byte to be one of the listed values.
requireOneOf :: [Word8] -> Get ()
requireOneOf bs = do
  byte <- getWord8
  when (not $ byte `elem` bs) $
    fail $ printf "requireOneOf: got %d, expected one of %s" byte (show bs)

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

  -- Analyse header
  header <- B.hGetSome h 6
  when (B.length header /= 6 || not (("fH" `B.isPrefixOf` header) ||
                                     ("fJ" `B.isPrefixOf` header))) $
    fail $ printf "Got invalid header: '%s'" (show header)
  
  let bodyLength = flip runGet (BL.fromStrict header) $ do
        skip 2
        getWord32le >>= return . fromIntegral
  let compressed = "fJ" `B.isPrefixOf` header

  -- Read body
  body <- B.hGetSome h bodyLength
  when (B.length body /= bodyLength) $
    fail $ printf "Length mismatch: header field %d; actual body %d" bodyLength (B.length body)

  remain <- B.hGetNonBlocking h 64 -- Grab a bit, hopefully enough to figure out what it is
  when (B.length remain /= 0) $
    fail $ printf "Length mismatch: data after given body length: %s" (show remain)

  -- Decompress body if necessary
  if not compressed
    then return body
    else do
    -- First 4 bytes are decompressed body length
    let lbody = BL.fromStrict body
        decompressedBodyLength = flip runGet (BL.take 4 lbody) $ do
          getWord32le >>= return . fromIntegral
        decompressed = decompress $ BL.drop 4 lbody

    when (BL.length decompressed /= fromIntegral decompressedBodyLength) $
      fail $ printf "Length mismatch: header field %d; decompressed body %d" bodyLength (BL.length decompressed)

    return $ BL.toStrict decompressed


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
