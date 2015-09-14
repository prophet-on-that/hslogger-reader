{-# LANGUAGE OverloadedStrings #-}

module Arguments
  ( Arguments (..)
  , opts
  ) where

import Options.Applicative
import Data.Time
import System.Log.Logger
import qualified Data.Text as T

data Arguments = Arguments
  { lowerTime :: Maybe UTCTime
  , upperTime :: Maybe UTCTime
  , lowerPrio :: Priority
  , upperPrio :: Priority
  , insensitive :: Bool
  , pattern :: Maybe T.Text
  , format :: T.Text
  , pid :: Maybe Int
  , tid :: Maybe Int
  , logFile :: FilePath
  }

parseArgs :: Parser Arguments
parseArgs
  = Arguments
      <$> ( optional . option auto $
              ( short 'l'
             <> long "lower-time"
             <> metavar "UTCTIME"
             <> help "Ignore messages logged before this time."
              )
          )
      <*> ( optional . option auto $
              ( short 'u'
             <> long "upper-time"
             <> metavar "UTCTIME"
             <> help "Ignore messages logged after this time."
              )
           )
      <*> ( option auto $
              ( short 'm'
             <> long "min-priority"
             <> value DEBUG
             <> showDefault
             <> metavar "PRIO"
             <> help "Assert a minimum message priority."
              )
          )
      <*> ( option auto $
              ( short 'n'
             <> long "max-priority"
             <> value EMERGENCY
             <> showDefault
             <> metavar "PRIO"
             <> help "Assert a maximum message priority."
              )
          )
      <*> ( switch $
              ( short 'i'
             <> long "insensitive"
             <> help "Ignore case when matching message pattern."
              )
          )
      <*> ( optional . option auto $
              ( short 'p'
             <> long "pattern"
             <> metavar "REGEXP"
             <> help "Assert a pattern the message must satisfy."
              )
          )
      <*> ( option auto $
              ( short 'f'
             <> long "format"
             <> value "[$utcTime $loggername $prio] $msg"
             <> showDefault
             <> metavar "FORMAT"
             <> help "Specify hslogger-style format of log files."
              )
          )
      <*> ( option auto $
              ( long "process-id"
             <> metavar "PID"
             <> help "Assert logging process."
              )
          )
      <*> ( option auto $
              ( long "thread-id"
             <> metavar "TID"
             <> help "Assert logging thread."
              )
          )
      <*> ( argument auto $ metavar "FILE"
          )

opts
  = info (helper <*> parseArgs)
      ( fullDesc
     <> progDesc "Filter hslogger-produced log files."
      )

      
