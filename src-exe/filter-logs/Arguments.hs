{-# LANGUAGE OverloadedStrings #-}

module Arguments
  ( Arguments (..)
  , opts
  ) where

import Options.Applicative
import Data.Time
import Data.Monoid
import System.Log.Logger
import qualified Data.Text as T

data Arguments = Arguments
  { lowerTime :: Maybe UTCTime
  , upperTime :: Maybe UTCTime
  , lowerPrio :: Priority
  , upperPrio :: Priority
  , outfile :: Maybe FilePath
  , logfile :: FilePath
  , insensitive :: Bool
  , pattern :: Maybe String
  , format :: T.Text
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
      <*> ( optional . option auto $
              ( short 'o'
             <> long "output-file"
             <> metavar "FILE"
             <> help "Write filtered messages to given file."
              )
          )
      <*> ( argument auto $ metavar "FILE"
          )
      <*> ( switch $
              ( short 'i'
             <> long "insensitive"
             <> help "Ignore case when matching message pattern."
              )
          )
      <*> ( optional . strOption $
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

opts
  = info (helper <*> parseArgs)
      ( fullDesc
     <> progDesc "Filter hslogger-produced log files."
      )

      
