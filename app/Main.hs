{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as BS

import Problems

main :: IO ()
main = do
  args <- getArgs
  input <- BS.getContents
  let result = do problemStr <- safeHead args
                  problem <- selectProblem problemStr
                  return $ problem input
  case result of
    Just output -> BS.putStrLn output
    Nothing -> BS.putStrLn "Problem not recognised"




selectProblem :: String -> Maybe Problem
selectProblem "dna" = Just dna
selectProblem "rna" = Just rna
selectProblem "revc" = Just revc
selectProblem _      = Nothing


safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a:_) = Just a
