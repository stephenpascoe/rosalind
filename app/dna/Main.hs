{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as BS

data BaseCount = BaseCount { aCount :: Integer
                           , cCount :: Integer
                           , gCount :: Integer
                           , tCount :: Integer
                           } deriving Show

main :: IO ()
main = do
  input <- BS.getContents
  let counts = countBases input
  putStrLn $ show (aCount counts) ++ " " ++
             show (cCount counts) ++ " " ++
             show (gCount counts) ++ " " ++
             show (tCount counts)

countBases :: BS.ByteString -> BaseCount
countBases bases = BS.foldl' f acc bases where
  acc = BaseCount 0 0 0 0
  f acc char = case char of
    'A' -> acc { aCount = (aCount acc) + 1 }
    'C' -> acc { cCount = (cCount acc) + 1 }
    'G' -> acc { gCount = (gCount acc) + 1 }
    'T' -> acc { tCount = (tCount acc) + 1 }
    -- We ignore any non ACGT bases.  This makes handling any newlines easier.
    _   -> acc
