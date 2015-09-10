-- | Compute number of lines and maximum message length for given log
-- file and format.

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
import System.Exit
import System.Environment

main = do
  (fileName, format) <- handleArgs
  lts <- fmap L.lines $ L.readFile fileName
  let
    result = do
      parser <- logMessageParser format loggerNameParser
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

    handleArgs :: IO (FilePath, T.Text)
    handleArgs = do
      args <- getArgs
      case args of
        [fileName, format] ->
          return (fileName, T.pack format)
        _ -> do
          name <- getProgName
          putStrLn $ "Usage: " ++ name ++ " format_string path/to/log/file"
          exitFailure
         
