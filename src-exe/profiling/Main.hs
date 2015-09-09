{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (takeWhile)
import System.Log.Reader
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as L
import Data.Attoparsec.Text.Lazy
import Data.Foldable
import Data.Maybe

main = do
  lt <- L.readFile "example.log"
  let
    logs
      = parseLogs loggerNameParser "[$utcTime $loggername $prio] $msg" lt
  case logs of
    Left err ->
      putStrLn err
    Right lms -> do
      let
        (lineCount, maxMsgLength)
          = foldl' f (0, 0) lms
      putStrLn $ "Lines: " ++ show lineCount ++ "\tMaximum message length: " ++ show maxMsgLength
  where
    loggerNameParser
      = takeTill isHorizontalSpace

    f (lineCount, maxMsgLength) lm
      = (lineCount + 1, newMaxLength)
      where
        newMaxLength
          = fromMaybe maxMsgLength $ do
              msg <- message lm
              return $ max maxMsgLength (T.length msg)
  
    
    
