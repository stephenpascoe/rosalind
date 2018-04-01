{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Encoding as TE
import Safe


import Problems

main :: IO ()
main = do
  args <- getArgs
  input <- BS.getContents
  let problemMay = do problemStr <- headMay args
                      selectProblem problemStr
      result = case problemMay of
                 Nothing      -> Left "Problem not recognised"
                 Just problem -> problem input
  printResult result


printResult :: Result -> IO ()
printResult result = case result of
  Left error -> do BS.putStr "ERROR: "
                   BS.putStrLn . TE.encodeUtf8 $ error
  Right output -> BS.putStrLn output



selectProblem :: String -> Maybe Problem
selectProblem "dna" = Just dna
selectProblem "rna" = Just rna
selectProblem "revc" = Just revc
selectProblem "fib"  = Just fib
selectProblem "gc"   = Just gc
selectProblem "hamm" = Just hamm
selectProblem "iprb" = Just iprb
selectProblem _      = Nothing
