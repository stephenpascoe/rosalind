{-# LANGUAGE OverloadedStrings #-}

module Problems
    ( dna
    , rna
    , recv
    , Problem
    ) where

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Builder as B
import Data.Monoid

import Lib

type Problem = BS.ByteString -> BS.ByteString

dna :: Problem
dna input = let counts = countBases input
            in B.toLazyByteString ( B.integerDec (aCount counts) <> " " <>
                                    B.integerDec (cCount counts) <> " " <>
                                    B.integerDec (gCount counts) <> " " <>
                                    B.integerDec (tCount counts)
                                  )

rna :: Problem
rna input = BS.map transcribe input

recv :: Problem
recv input = let strands = BS.words input
             in  BS.reverse (BS.map complement (head strands))
