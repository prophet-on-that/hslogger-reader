{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Arguments

import Options.Applicative (execParser)
import Prelude hiding (takeWhile)
import System.Log.Reader
import System.Log.Logger
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import Data.Attoparsec.Text.Lazy
-- import Data.Foldable
import Data.Maybe
import System.Exit
import System.Environment
import Control.Applicative
import Data.Time
import Data.Text.ICU

main = do
  arguments <- execParser opts
  undefined

filterLogMessage
  :: Priority -- ^ Lower priority
  -> Priority -- ^ Upper priority
  -> Maybe UTCTime -- ^ Lower bound on date
  -> Maybe UTCTime -- ^ Upper bound on date
  -> Maybe Int -- ^ Process ID
  -> Maybe Int -- ^ Thread ID
  -> Maybe Regex
  -> LogMessage
  -> Bool
filterLogMessage lowerPrio upperPrio lTime uTime pid tid reg lm
  = and . catMaybes $
      [ (>= lowerPrio) <$> priority lm
      , (<= upperPrio) <$> priority lm
      , liftA2 (<=) lTime $ zonedTimeToUTC <$> timestamp lm
      , liftA2 (>=) uTime $ zonedTimeToUTC <$> timestamp lm
      , liftA2 (==) pid $ processId lm
      , liftA2 (==) tid $ threadId lm
      , liftA2 find reg (message lm) >> return True
      ]
