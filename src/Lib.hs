{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( complement
    , transcribe
    , countBases
    , BaseCount(..)
    ) where

import qualified Data.ByteString.Lazy.Char8 as BS


data BaseCount = BaseCount { aCount :: Integer
                           , cCount :: Integer
                           , gCount :: Integer
                           , tCount :: Integer
                           } deriving Show



complement :: Char -> Char
complement char = case char of
  'A' -> 'T'
  'T' -> 'A'
  'C' -> 'G'
  'G' -> 'C'
  _   -> error $ "Not a base: " ++ [char]

transcribe :: Char -> Char
transcribe char = case char of
  'T' -> 'U'
  _   -> char

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
