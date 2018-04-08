{-# LANGUAGE OverloadedStrings #-}

module Lib.Codon ( translate
                 ) where

import qualified Data.ByteString.Char8 as BS

import Lib.Sequence

-- | Translate an RNA sequence into a Protein sequence
--   TODO : use Phantom types to make this only work with RNA
translate :: Sequence -> Sequence
translate (RnaSeq seq) = ProteinSeq . BS.pack $ translate' seq
  where
    translate' seq =
      let (prefix, rest) = BS.splitAt 3 seq
          charMay = codonTable prefix
      in case charMay of
        Just char -> (char:translate' rest)
        Nothing   -> []


translate _ = error "Not RNA"

codonTable :: BS.ByteString -> Maybe Char
codonTable c | c == "UUU" = Just 'F'
codonTable c | c == "CUU" = Just 'L'
codonTable c | c == "AUU" = Just 'I'
codonTable c | c == "GUU" = Just 'V'
codonTable c | c == "UUC" = Just 'F'
codonTable c | c == "CUC" = Just 'L'
codonTable c | c == "AUC" = Just 'I'
codonTable c | c == "GUC" = Just 'V'
codonTable c | c == "UUA" = Just 'L'
codonTable c | c == "CUA" = Just 'L'
codonTable c | c == "AUA" = Just 'I'
codonTable c | c == "GUA" = Just 'V'
codonTable c | c == "UUG" = Just 'L'
codonTable c | c == "CUG" = Just 'L'
codonTable c | c == "AUG" = Just 'M'
codonTable c | c == "GUG" = Just 'V'
codonTable c | c == "UCU" = Just 'S'
codonTable c | c == "CCU" = Just 'P'
codonTable c | c == "ACU" = Just 'T'
codonTable c | c == "GCU" = Just 'A'
codonTable c | c == "UCC" = Just 'S'
codonTable c | c == "CCC" = Just 'P'
codonTable c | c == "ACC" = Just 'T'
codonTable c | c == "GCC" = Just 'A'
codonTable c | c == "UCA" = Just 'S'
codonTable c | c == "CCA" = Just 'P'
codonTable c | c == "ACA" = Just 'T'
codonTable c | c == "GCA" = Just 'A'
codonTable c | c == "UCG" = Just 'S'
codonTable c | c == "CCG" = Just 'P'
codonTable c | c == "ACG" = Just 'T'
codonTable c | c == "GCG" = Just 'A'
codonTable c | c == "UAU" = Just 'Y'
codonTable c | c == "CAU" = Just 'H'
codonTable c | c == "AAU" = Just 'N'
codonTable c | c == "GAU" = Just 'D'
codonTable c | c == "UAC" = Just 'Y'
codonTable c | c == "CAC" = Just 'H'
codonTable c | c == "AAC" = Just 'N'
codonTable c | c == "GAC" = Just 'D'
codonTable c | c == "UAA" = Nothing
codonTable c | c == "CAA" = Just 'Q'
codonTable c | c == "AAA" = Just 'K'
codonTable c | c == "GAA" = Just 'E'
codonTable c | c == "UAG" = Nothing
codonTable c | c == "CAG" = Just 'Q'
codonTable c | c == "AAG" = Just 'K'
codonTable c | c == "GAG" = Just 'E'
codonTable c | c == "UGU" = Just 'C'
codonTable c | c == "CGU" = Just 'R'
codonTable c | c == "AGU" = Just 'S'
codonTable c | c == "GGU" = Just 'G'
codonTable c | c == "UGC" = Just 'C'
codonTable c | c == "CGC" = Just 'R'
codonTable c | c == "AGC" = Just 'S'
codonTable c | c == "GGC" = Just 'G'
codonTable c | c == "UGA" = Nothing
codonTable c | c == "CGA" = Just 'R'
codonTable c | c == "AGA" = Just 'R'
codonTable c | c == "GGA" = Just 'G'
codonTable c | c == "UGG" = Just 'W'
codonTable c | c == "CGG" = Just 'R'
codonTable c | c == "AGG" = Just 'R'
codonTable c | c == "GGG" = Just 'G'
codonTable _ = Nothing
