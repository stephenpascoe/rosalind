{-# LANGUAGE OverloadedStrings #-}

module Problems
    ( dna
    , rna
    , revc
    , Problem
    ) where

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Builder as B
import Data.Monoid
import Data.Maybe

import Lib

type Problem = BS.ByteString -> BS.ByteString

dna :: Problem
dna input = maybe "Not a DNA sequence" f (toDnaSequence input)
  where
    f seq = let counts = countBases seq
            in B.toLazyByteString ( B.integerDec (aCount counts) <> " " <>
                                    B.integerDec (cCount counts) <> " " <>
                                    B.integerDec (gCount counts) <> " " <>
                                    B.integerDec (tCount counts)
                                  )

rna :: Problem
rna input =
  let
    result = do
      seq <- toDnaSequence input
      return $ seqAsByteString . transcribe $ seq
  in
    case result of
      Nothing -> "Something went wrong"
      Just output -> BS.fromStrict output

revc :: Problem
revc input = let strands = BS.words input
                 f = BS.reverse . BS.fromStrict . seqAsByteString . complement
             in maybe "Not a Strand" f (toDnaSequence (head strands))
