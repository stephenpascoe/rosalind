{-# LANGUAGE OverloadedStrings #-}

module Problems
    ( Problem
    , Result
    , dna
    , rna
    , revc
    , fib
    , gc
    , hamm
    ) where

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Builder as B
import Data.Monoid
import Data.Maybe
import Safe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.List
import Control.Monad

import Lib
import Lib.Fasta (parseFasta, mapSeq)

-- | A Problem takes an input stream and returns either an error message or an output stream.
--   Streams are represented as lazy bytestrings.
type Result = Either T.Text BS.ByteString
type Problem = BS.ByteString -> Result

dna :: Problem
dna input = case toDnaSequence input of
  Just seq -> Right (serialise (countBases seq))
  Nothing ->  Left "Not a DNA sequence"
  where
    serialise counts = builderToBS $ B.integerDec (aCount counts) <> " " <>
                                                           B.integerDec (cCount counts) <> " " <>
                                                           B.integerDec (gCount counts) <> " " <>
                                                           B.integerDec (tCount counts)

rna :: Problem
rna input = case toDnaSequence input of
  Just seq -> Right $ seqAsByteString $ transcribe seq
  Nothing  -> Left "Not a DNA sequence"

revc :: Problem
revc input = case toDnaSequence input of
  Just seq -> Right $ BS.reverse . seqAsByteString . complement $ seq
  Nothing  -> Left "Not a DNA sequence"

fib :: Problem
fib input = case resultMay of
              Just pairs -> Right $ builderToBS . B.intDec $ pairs
              Nothing -> Left "Parse error"
  where
    args = BS.words input
    resultMay = do
      nStr <- headMay args
      kStr <- atMay args 1
      (n, _) <- BS.readInt nStr
      (k, _) <- BS.readInt kStr
      return $ (rabbitPairs k) !! (n - 1)


gc :: Problem
gc input = case parseFasta input of
  Right fasta -> let
    cmp' (l1, v1) (l2, v2) = compare v1 v2
    (label, maxGC) = maximumBy cmp' (mapSeq gcContent fasta)
    in
      Right $ BS.concat [TE.encodeUtf8 label, "\n", BS.pack $ show (maxGC * 100)]

  Left err -> Left $ T.pack err

hamm :: Problem
hamm input = do
   let lines = BS.lines input
       distanceMay = do
         seqStr1 <- headMay lines
         seqStr2 <- headMay <=< tailMay $ lines
         seq1 <- toDnaSequence seqStr1
         seq2 <- toDnaSequence seqStr2
         return $ hammingDistance seq1 seq2
   case distanceMay of
     Just result -> Right $ BS.pack $ show result
     Nothing     -> Left "Parser error"


-- Utilities

builderToBS = BSL.toStrict . B.toLazyByteString
