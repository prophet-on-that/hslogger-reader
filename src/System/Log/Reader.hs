{-# LANGUAGE OverloadedStrings #-}

module System.Log.Reader
  ( parseLog
  , tfParseLog
    -- * Utilities
  , logMessageParser
  , FormatString
  , zonedTimeParser
  ) where

import Prelude hiding (takeWhile)
import Data.Attoparsec.Text.Lazy 
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Control.Applicative
import Data.Foldable
import System.Log
import Data.Time
import Data.Char (isAlpha)
import Data.Monoid
import System.Locale (defaultTimeLocale)

type FormatString = T.Text

data LogMessage = LogMessage
  { message :: !(Maybe T.Text)
  , loggerName :: !(Maybe T.Text)
  , priority :: !(Maybe Priority)
  , threadId :: !(Maybe Int)
  , processId :: !(Maybe Int)
  , timestamp :: !(Maybe ZonedTime)
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
      instr <- handleNoise <|> do
        char '$'
        instr <- takeWhile isAlpha
        case instr of
          "msg" -> return Message
          "loggername" -> return LoggerName
          "prio" -> return Priority
          "tid" -> return ThreadId
          "pid" -> return ProcessId
          "time" -> return Time
          "utcTime" -> return TimeUTC
          str -> fail $ "Formatter `" ++ T.unpack str ++ "' is unsupported, use one of $msg, $loggername, $prio, $tid, $pid, $time, $utcTime"
      (instr :) <$> formatStringParser
  where
    handleEOI :: Parser [Instruction]
    handleEOI = do
      endOfInput
      return []

    handleNoise = do
      noise <- takeWhile1 (/= '$')
      return $ Noise noise

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
      return $ lm { timestamp = Just time' }
    helper lm TimeUTC = do
      time' <- zonedTimeParser
      return $ lm { timestamp = Just time' }

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

-- | Parse a log message, as outputted by hslogger. 
parseLog
  :: Parser T.Text -- ^ LoggerName parser
  -> FormatString
  -> L.Text 
  -> Either String LogMessage
parseLog 
  = tfParseLog zonedTimeParser

-- | As 'parseLog', but specify time parser (for compatibility with
-- 'tfLogFormatter').
tfParseLog
  :: Parser ZonedTime -- ^ Time parser
  -> Parser T.Text -- ^ LoggerName parser
  -> FormatString
  -> L.Text 
  -> Either String LogMessage
tfParseLog zonedTimeParser loggerNameParser format log = do
  parser <- logMessageParser format loggerNameParser zonedTimeParser
  eitherResult $ parse parser log

-- | Parse time format string @ "%F %X %Z" @ with 'defaultTimeLocale'.
zonedTimeParser :: Parser ZonedTime
zonedTimeParser = do
  date <- takeWhile (inClass "-0-9")
  skipSpace
  time <- takeWhile (inClass ":0-9")
  skipSpace
  tzName <- takeWhile isAlpha
  let
    str
      = date `space` time `space` tzName
        
  case parseTime defaultTimeLocale "%F %X %Z" (T.unpack str) of
    Nothing ->
      fail "zonedTimeParse: failed to parse ZonedTime"
    Just zt ->
      return zt
  where
    space a b
      = a <> " " <> b
    
