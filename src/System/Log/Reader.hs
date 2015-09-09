{-# LANGUAGE OverloadedStrings #-}

module System.Log.Reader
  ( parseLogs
  -- * Utilities
  , logMessageParser
  , FormatString
  ) where

import Data.Attoparsec.Text.Lazy 
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Control.Applicative
import Data.Foldable
import System.Log
import Data.Time

type FormatString = T.Text

data LogMessage = LogMessage
  { message :: !(Maybe T.Text)
  , loggerName :: !(Maybe T.Text)
  , priority :: !(Maybe Priority)
  , threadId :: !(Maybe Int)
  , processId :: !(Maybe Int)
  , time :: !(Maybe ZonedTime)
  , timeUTC :: !(Maybe UTCTime)
  } deriving (Show)

logMessage :: LogMessage
logMessage
  = LogMessage
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing

data Instruction
  = Noise T.Text
  | Message
  | LoggerName
  | Priority
  | ThreadId
  | ProcessId
  | Time
  | TimeUTC
  deriving (Show)

formatStringParser :: Parser [Instruction]
formatStringParser
  = handleEOI <|> do
      instruction <- handleNoise <|>
                     handleMessage <|>
                     handleLoggerName <|>
                     handlePriority <|>
                     handleThreadId <|>
                     handleProcessId <|>
                     handleTime <|> 
                     handleTimeUTC <|>
                     fail "Saw unsupported formatter, use one of msg, loggername, prio, tid, pid, time, utcTime"
      (instruction :) <$> formatStringParser
  where
    handleEOI :: Parser [Instruction]
    handleEOI = do
      endOfInput
      return []

    formatChar
      = '$'
      
    handleNoise = do
      noise <- takeWhile1 (/= formatChar)
      return $ Noise noise

    handleMessage = do
      char formatChar
      "msg"
      return Message

    handleLoggerName = do
      char formatChar
      "loggername"
      return LoggerName

    handlePriority = do
      char formatChar
      "prio"
      return Priority

    handleThreadId = do
      char formatChar
      "tid"
      return ThreadId

    handleProcessId = do
      char formatChar
      "pid"
      return ProcessId

    handleTime = do
      char formatChar
      "time"
      return Time

    handleTimeUTC = do
      char formatChar
      "utcTime"
      return TimeUTC

buildParser
  :: Parser T.Text -- ^ LoggerName parser
  -> Parser ZonedTime
  -> [Instruction]
  -> Parser LogMessage
buildParser loggerNameParser zonedTimeParser
  = foldlM helper logMessage
  where
    helper :: LogMessage -> Instruction -> Parser LogMessage
    helper lm (Noise noise) = do
      _ <- string noise
      return lm
    helper lm Message = do
      msg <- takeTill isEndOfLine
      return $ lm { message = Just msg }
    helper lm LoggerName = do
      name <- loggerNameParser
      return $ lm { loggerName = Just name }
    helper lm Priority = do
      prio <- parsePriority
      return $ lm { priority = Just prio }
      where
        parsePriority :: Parser Priority
        parsePriority
          = ("DEBUG" >> return DEBUG) <|>
            ("INFO" >> return INFO) <|>
            ("NOTICE" >> return NOTICE) <|> 
            ("WARNING" >> return WARNING) <|> 
            ("ERROR" >> return ERROR) <|> 
            ("CRITICAL" >> return CRITICAL) <|> 
            ("ALERT" >> return ALERT) <|> 
            ("EMERGENCY" >> return EMERGENCY) <|>
            (fail "Priority not recognised")
    helper lm ThreadId = do
      tid <- decimal
      return $ lm { threadId = Just tid }
    helper lm ProcessId = do
      pid <- decimal
      return $ lm { processId = Just pid }
    helper lm Time = do
      time' <- zonedTimeParser
      return $ lm { time = Just time' }
    helper lm TimeUTC = do
      time' <- zonedTimeParser
      let
        time''
          = zonedTimeToUTC time'
      return $ lm { timeUTC = Just time'' }

-- | Build a parser for a 'LogMessage' from a format string, as
-- described by the hslogger package. 
logMessageParser
  :: FormatString
  -> Parser T.Text -- ^ LoggerName parser
  -> Parser ZonedTime -- ^ Time parser
  -> Either String (Parser LogMessage)
logMessageParser format loggerNameParser zonedTimeParser = do
  instrs <- parseOnly (formatStringParser <* endOfInput) format
  return $ buildParser loggerNameParser zonedTimeParser instrs

parseLogs
  :: FormatString
  -> Parser T.Text -- ^ LoggerName parser
  -> Parser ZonedTime -- ^ Time parser
  -> L.Text 
  -> Either String [LogMessage]
parseLogs format loggerNameParser zonedTimeParser logs = do
  parser <- logMessageParser format loggerNameParser zonedTimeParser
  eitherResult $ parse (sepBy' parser endOfLine) logs
