{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as BS

import Lib

main :: IO ()
main = do
  input <- BS.getContents
  BS.putStrLn $ BS.map transcribe input
