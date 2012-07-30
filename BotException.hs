{-# LANGUAGE DeriveDataTypeable #-}

module BotException where

import Control.Exception
import Data.Typeable
import Text.Printf


data BotException = FailSilent
                  | FailMessage String
                  deriving (Show, Typeable)

instance Exception BotException


failSilent = throw FailSilent
failMsg = throw . FailMessage
