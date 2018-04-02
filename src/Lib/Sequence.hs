{-# LANGUAGE OverloadedStrings #-}

module Lib.Sequence  ( toByteString
                     , fromByteString
                     , dnaFromByteString
                     , rnaFromByteString
                     , proteinFromByteString
                     , Sequence(..)
                     )
where


import qualified Data.ByteString.Char8 as BS
import Control.Applicative


data Sequence = DnaSeq BS.ByteString
              | RnaSeq BS.ByteString
              | ProteinSeq BS.ByteString
              deriving Show

toByteString :: Sequence -> BS.ByteString
toByteString (DnaSeq seq) = seq
toByteString (RnaSeq seq) = seq
toByteString (ProteinSeq seq) = seq

fromByteString :: BS.ByteString -> Maybe Sequence
fromByteString bs = dnaFromByteString bs <|> rnaFromByteString bs <|> proteinFromByteString bs

dnaFromByteString :: BS.ByteString -> Maybe Sequence
dnaFromByteString bs = DnaSeq <$> validate "ATCG" bs

rnaFromByteString :: BS.ByteString -> Maybe Sequence
rnaFromByteString bs = RnaSeq <$> validate "AUCG" bs

proteinFromByteString :: BS.ByteString -> Maybe Sequence
proteinFromByteString bs = ProteinSeq <$> validate protVocab bs
  where
    protVocab = BS.filter (\c -> not (BS.elem c "BJOUXZ")) $ BS.pack ['A'..'Z']

validate vocab bs = if BS.all (\c -> BS.elem c vocab) bs then
                      Just bs else Nothing
