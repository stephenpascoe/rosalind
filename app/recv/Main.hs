{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as BS

import Lib

main :: IO ()
main = do
  input <- BS.getContents
  let
    strands = BS.words input
  BS.putStrLn $ BS.reverse (BS.map complement (head strands))
