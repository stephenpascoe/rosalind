{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( complement
    , transcribe
    ) where

import qualified Data.ByteString.Char8 as BS


complement :: Char -> Char
complement char = case char of
  'A' -> 'T'
  'T' -> 'A'
  'C' -> 'G'
  'G' -> 'C'
  _   -> error $ "Not a base: " ++ [char]

transcribe :: Char -> Char
transcribe char = case char of
  'T' -> 'U'
  _   -> char
