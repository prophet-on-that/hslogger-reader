{-# LANGUAGE CPP #-}

module Arguments
  ( Arguments (..)
  , opts
  ) where

import Options.Applicative
import System.Log.Logger
import qualified Data.Text as T
import Data.Maybe
import Data.Time
#if MIN_VERSION_time(1,5,0)
#else
import System.Locale (defaultTimeLocale)
#endif

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
  , noContinue :: Bool
  , logFile :: FilePath
  }

parseArgs :: Parser Arguments
parseArgs
  = Arguments
      <$> ( optional . fmap parseUTCTime . strOption $
              ( short 'l'
             <> long "lower-time"
             <> metavar "UTCTIME"
             <> help "Ignore messages logged before this time."
              )
          )
      <*> ( optional . fmap parseUTCTime . strOption $
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
      <*> ( optional . fmap T.pack . strOption $
              ( short 'p'
             <> long "pattern"
             <> metavar "REGEXP"
             <> help "Assert a pattern the logger name must satisfy."
              )
          )
      <*> ( fmap T.pack . strOption $
              ( short 'f'
             <> long "format"
             <> value "[$utcTime $loggername $prio] $msg"
             <> showDefault
             <> metavar "FORMAT"
             <> help "Specify hslogger-style format of log files."
              )
          )
      <*> ( optional . option auto $
              ( long "process-id"
             <> metavar "PID"
             <> help "Assert logging process."
              )
          )
      <*> ( optional . option auto $
              ( long "thread-id"
             <> metavar "TID"
             <> help "Assert logging thread."
              )
          )
      <*> ( switch $
              ( long "no-continue"
             <> help "Do not skip unparsable lines, but fail with error message."
              )
          )
      <*> ( strArgument $ metavar "FILE"
          )

opts
  = info (helper <*> parseArgs)
      ( header "Filter hslogger-produced log files."
     <> progDesc "Unparsable lines will be skipped. This behaviour can be overridden with the --no-continue option."
     <> fullDesc
     <> footer "Limitations: logs must use hslogger's default time format `yyyy-mm-ddThh:mm:ssZ' and logger names must not include whitespace."
      )

parseUTCTime :: String -> UTCTime
parseUTCTime
#if MIN_VERSION_time(1,5,0)
  = fromMaybe (error "Cannot parse date (expected format `yyyy-mm-ddThh:mm:ssZ'") . parseTimeM True defaultTimeLocale "%FT%TZ"
#else
  = fromMaybe (error "Cannot parse date (expected format `yyyy-mm-ddThh:mm:ssZ'") . parseTime defaultTimeLocale "%FT%TZ"
#endif
