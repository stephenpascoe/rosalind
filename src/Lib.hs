{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( toDnaSequence
  , toRnaSequence
  , seqAsByteString
  , complement
  , transcribe
  , countBases
  , BaseCount(..)
  ) where

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BS
import Control.Applicative

data BaseCount = BaseCount { aCount :: Integer
                           , cCount :: Integer
                           , gCount :: Integer
                           , tCount :: Integer
                           , uCount :: Integer
                           } deriving Show

data Sequence = DnaSeq BS.ByteString
              | RnaSeq BS.ByteString
              deriving Show


type ProblemInput = BSL.ByteString

toDnaSequence :: ProblemInput -> Maybe Sequence
toDnaSequence bs = DnaSeq <$> _checkSequence "ATCG" bs
toRnaSequence bs = RnaSeq <$> _checkSequence "AUCG" bs

_checkSequence :: BS.ByteString -> ProblemInput -> Maybe BS.ByteString
_checkSequence vocab bs = if BS.all check bs' then
                            Just bs'
                          else
                            Nothing
  where
    bs' = BSL.toStrict bs
    check char = BS.elem char vocab


seqAsByteString :: Sequence -> BS.ByteString
seqAsByteString (DnaSeq seq) = seq
seqAsByteString (RnaSeq seq) = seq

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
