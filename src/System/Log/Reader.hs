-- | Generate a parser for logs produced by the
-- <http://hackage.haskell.org/package/hslogger hslogger> package,
-- supporting arbitrary formatting strings (see
-- "System.Log.Formatter"). Currently, this package does not provide
-- support for custom-defined formatters.
--
-- 'logMessageParser' will generate a parser for a 'LogMessage' given
-- a format string (such @ "[$utcTime $loggername $prio] $msg" @) and
-- a parser for your logger names. This can then be used to read logs
-- line-by-line, potentially in constant memory, from disk. See the
-- accompanying executable for an example of this.

{-# LANGUAGE OverloadedStrings #-}

module System.Log.Reader
  ( LogMessage (..)
  , FormatString
  , logMessageParser
  , tfLogMessageParser
    -- * Extras
  , zonedTimeParser
  ) where

import Prelude hiding (takeWhile)
import Data.Attoparsec.Text.Lazy 
import qualified Data.Text as T
import Control.Applicative
import Data.Foldable
import System.Log
import Data.Char (isAlpha)
import Data.Monoid
import Data.Time
#if MIN_VERSION_time(1,5,0)
#else
import System.Locale (defaultTimeLocale)
#endif

type FormatString = T.Text

data LogMessage = LogMessage
  { message :: Maybe T.Text
  , loggerName :: Maybe T.Text
  , priority :: Maybe Priority
  , threadId :: Maybe Int
  , processId :: Maybe Int
  , timestamp :: Maybe ZonedTime
  } deriving (Show)

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
  = foldlM helper $ LogMessage Nothing Nothing Nothing Nothing Nothing Nothing 
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
  -> Either String (Parser LogMessage)
logMessageParser format loggerNameParser
  = tfLogMessageParser format loggerNameParser zonedTimeParser

-- | As 'logMessageParser', but provide a custom time format for
-- parsing @ "$time" @ and @ "$utcTime" @ formatters. Compatible with
-- hslogger's /tfLogFormatter/ function.
tfLogMessageParser
  :: FormatString
  -> Parser T.Text -- ^ LoggerName parser
  -> Parser ZonedTime -- ^ Time parser
  -> Either String (Parser LogMessage)
tfLogMessageParser format loggerNameParser zonedTimeParser = do
  instrs <- parseOnly (formatStringParser <* endOfInput) format
  return $ buildParser loggerNameParser zonedTimeParser instrs

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

#if MIN_VERSION_time (1,5,0)        
  case parseTimeM True defaultTimeLocale "%F %X %Z" (T.unpack str) of
#else  
  case parseTime defaultTimeLocale "%F %X %Z" (T.unpack str) of
#endif
    Nothing ->
      fail "zonedTimeParse: failed to parse ZonedTime"
    Just zt ->
      return zt
  where
    space a b
      = a <> " " <> b
    
