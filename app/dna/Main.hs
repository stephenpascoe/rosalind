{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Builder as B
import Data.Monoid

import Lib

main :: IO ()
main = do
  input <- BS.getContents
  BS.putStrLn $ dna input


dna :: BS.ByteString -> BS.ByteString
dna input = let counts = countBases input
            in B.toLazyByteString ( B.integerDec (aCount counts) <> " " <>
                                    B.integerDec (cCount counts) <> " " <>
                                    B.integerDec (gCount counts) <> " " <>
                                    B.integerDec (tCount counts)
                                  )
