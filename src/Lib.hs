{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( toDnaSequence
  , toRnaSequence
  , seqAsByteString
  , complement
  , transcribe
  , countBases
  , BaseCount(..)
  , rabbitPairs
  , gcContent
  , hammingDistance
  ) where

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BS
import Control.Applicative
import Data.List

import Lib.Sequence

data BaseCount = BaseCount { aCount :: Integer
                           , cCount :: Integer
                           , gCount :: Integer
                           , tCount :: Integer
                           , uCount :: Integer
                           } deriving Show




complement :: Sequence -> Sequence
complement (DnaSeq seq) = DnaSeq $ BS.map f seq
  where
    f char = case char of
      'A' -> 'T'
      'T' -> 'A'
      'C' -> 'G'
      'G' -> 'C'
complement (RnaSeq seq) = RnaSeq $ BS.map f seq
  where
    f char = case char of
      'A' -> 'U'
      'U' -> 'A'
      'C' -> 'G'
      'G' -> 'C'

transcribe :: Sequence -> Sequence
transcribe (DnaSeq seq) = RnaSeq $ BS.map f seq
  where
    f char = case char of
      'T' -> 'U'
      _   -> char

-- TODO : funcs in terms of Sequence



countBases :: Sequence -> BaseCount
countBases (DnaSeq bases) = BS.foldl' f acc bases
  where
    acc = BaseCount 0 0 0 0 0
    f acc char = case char of
      'A' -> acc { aCount = (aCount acc) + 1 }
      'C' -> acc { cCount = (cCount acc) + 1 }
      'G' -> acc { gCount = (gCount acc) + 1 }
      'T' -> acc { tCount = (tCount acc) + 1 }
countBases (RnaSeq bases) = BS.foldl' f acc bases
  where
    acc = BaseCount 0 0 0 0 0
    f acc char = case char of
      'A' -> acc { aCount = (aCount acc) + 1 }
      'C' -> acc { cCount = (cCount acc) + 1 }
      'G' -> acc { gCount = (gCount acc) + 1 }
      'U' -> acc { uCount = (uCount acc) + 1 }



rabbitPairs :: Int -> [Int]
rabbitPairs k = 1 : 1 : zipWith f fib (tail $ fib)
  where fib = rabbitPairs k
        f g1 g2 = g1 * k + g2


gcContent :: Sequence -> Float
gcContent seq = fromIntegral (BS.foldl' f 0 seqStr) / fromIntegral (BS.length seqStr)
  where
    seqStr = seqAsByteString seq
    f acc char = case char of
                   'G' -> acc + 1
                   'C' -> acc + 1
                   _   -> acc

hammingDistance :: Sequence -> Sequence -> Int
hammingDistance seq1 seq2 = foldl' f 0 (BS.zip str1 str2)
  where
    str1 = seqAsByteString seq1
    str2 = seqAsByteString seq2
    f acc (char1, char2) = if char1 == char2 then acc else acc + 1
