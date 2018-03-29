{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as BS

import Lib

main :: IO ()
main = do
  input <- BS.getContents
  BS.putStrLn $ recv input

recv :: BS.ByteString -> BS.ByteString
recv input = let strands = BS.words input
             in  BS.reverse (BS.map complement (head strands))
