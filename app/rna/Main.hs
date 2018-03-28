{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
  input <- BS.getContents
  BS.putStrLn $ BS.map transcribe input

transcribe :: Char -> Char
transcribe char = case char of
  'T' -> 'U'
  _   -> char
