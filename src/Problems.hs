{-# LANGUAGE OverloadedStrings #-}

module Problems
    ( Problem
    , Result
    , dna
    , rna
    , revc
    , fib
    ) where

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Builder as B
import Data.Monoid
import Data.Maybe
import Safe
import qualified Data.Text as T

import Lib

-- | A Problem takes an input stream and returns either an error message or an output stream.
--   Streams are represented as lazy bytestrings.
type Result = Either T.Text BS.ByteString
type Problem = BS.ByteString -> Result

dna :: Problem
dna input = case toDnaSequence input of
  Just seq -> Right (serialise (countBases seq))
  Nothing ->  Left "Not a DNA sequence"
  where
    serialise counts = B.toLazyByteString ( B.integerDec (aCount counts) <> " " <>
                                            B.integerDec (cCount counts) <> " " <>
                                            B.integerDec (gCount counts) <> " " <>
                                            B.integerDec (tCount counts)
                                          )

rna :: Problem
rna input = case toDnaSequence input of
  Just seq -> Right $ BS.fromStrict . seqAsByteString $ transcribe seq
  Nothing  -> Left "Not a DNA sequence"

revc :: Problem
revc input = case toDnaSequence input of
  Just seq -> Right $ BS.reverse . BS.fromStrict . seqAsByteString . complement $ seq
  Nothing  -> Left "Not a DNA sequence"

fib :: Problem
fib input = case resultMay of
              Just pairs -> Right $ B.toLazyByteString . B.intDec $ pairs
              Nothing -> Left "Parse error"
  where
    args = BS.words input
    resultMay = do
      nStr <- headMay args
      kStr <- atMay args 1
      (n, _) <- BS.readInt nStr
      (k, _) <- BS.readInt kStr
      return $ (rabbitPairs k) !! (n - 1)
