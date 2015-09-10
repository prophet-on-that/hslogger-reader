{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Prelude hiding (takeWhile)
import System.Log.Reader
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import Data.Attoparsec.Text.Lazy
import Data.Foldable
import Data.Maybe

main = do
  lts <- fmap L.lines $ L.readFile "example.log"
  let
    result = do
      parser <- logMessageParser "[$utcTime $loggername $prio] $msg" loggerNameParser
      let
        f (!lineCount, !maxMsgLength) lt = do
          lm <- eitherResult . parse parser $ lt
          let
            newMaxLength
              = fromMaybe maxMsgLength $ do
                  msg <- message lm
                  return $ max maxMsgLength (T.length msg)
          return (lineCount + 1, newMaxLength)
      foldlM f (0, 0) lts
  case result of
    Left err ->
      putStrLn err
    Right (lineCount, maxMsgLength) ->  
      putStrLn $ "Lines: " ++ show lineCount ++ "\tMaximum message length: " ++ show maxMsgLength
  where
    loggerNameParser
      = takeTill isHorizontalSpace
