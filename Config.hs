{-# LANGUAGE OverloadedStrings #-}

module Config where

import Control.Applicative
import Control.Monad
import Data.ByteString
import Data.Yaml


data Config = Config { cIrcServer      :: String,
                       cIrcNick        :: String,
                       cIrcChannel     :: String,
                       
                       cLogName        :: String, 
                       cConnectTimeout :: Int,
                       cPollInterval   :: Int }
            deriving (Show)

instance FromJSON Config where
  parseJSON (Object v) = Config <$>
                         v .: "server" <*>
                         v .: "nick" <*>
                         v .: "channel" <*>
                         
                         v .: "logfile" <*>
                         v .: "connect-timeout" <*>
                         v .: "poll-interval"
  parseJSON _ = mzero
