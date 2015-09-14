{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Arguments

import Options.Applicative (execParser)
import Prelude hiding (takeWhile)
import System.Log.Reader
import System.Log.Logger
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import Data.Attoparsec.Text.Lazy
import Data.Maybe
import Control.Applicative
import Data.Time
import Data.Text.ICU hiding (pattern)
import Control.Exception
import Control.Monad
import Data.Typeable
import Data.Monoid

main = do
  arguments <- execParser opts

  regex'' <- case pattern arguments of
    Nothing ->
      return Nothing
    Just pattern' -> do
      let
        regexOptions
          = if insensitive arguments
              then
                [CaseInsensitive]
              else
                []
      either (throwIO . RegexError) (return . return) $ regex' regexOptions pattern'
  messageParser <- either (throwIO . ParserError) return $ logMessageParser (format arguments) loggerNameParser

  lts <- fmap L.lines . L.readFile . logFile $ arguments
  forM lts $ \lt -> do
    lm <- either (const $ throwIO (MessageParseError lt)) return . eitherResult . parse messageParser $ lt
    let
      pred
        = filterLogMessage
            (lowerPrio arguments)
            (upperPrio arguments)
            (lowerTime arguments)
            (upperTime arguments)
            (pid arguments)
            (tid arguments)
            regex''
            
    when (pred lm) $ L.putStrLn lt
  where
    loggerNameParser
      = takeTill isHorizontalSpace

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
      , do
          reg' <- reg
          name <- loggerName lm
          return . fromMaybe False $ find reg' name >> return True
      ]

data Errors
  = MessageParseError L.Text
  | RegexError ParseError
  | ParserError String
  deriving (Typeable)

instance Show Errors where
  show (MessageParseError lt)
    = L.unpack $ "Format of following message does not match expected: " <> lt
  show (RegexError err)
    = "Failure parsing regular expression: " <> show err
  show (ParserError str)
    = "Failure parsing log format spec: " <> str
    
instance Exception Errors

