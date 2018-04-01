{-# LANGUAGE OverloadedStrings #-}

module Lib.Sequence ( Sequence(..)
                    , seqAsByteString
                    , toDnaSequence
                    , toRnaSequence
                    ) where


import qualified Data.ByteString.Char8 as BS


data Sequence = DnaSeq BS.ByteString
              | RnaSeq BS.ByteString
              deriving Show


seqAsByteString :: Sequence -> BS.ByteString
seqAsByteString (DnaSeq seq) = seq
seqAsByteString (RnaSeq seq) = seq


toDnaSequence :: BS.ByteString -> Maybe Sequence
toDnaSequence bs = DnaSeq <$> _checkSequence "ATCG" bs
toRnaSequence bs = RnaSeq <$> _checkSequence "AUCG" bs

_checkSequence :: BS.ByteString -> BS.ByteString -> Maybe BS.ByteString
_checkSequence vocab bs = if BS.all check bs then
                            Just bs
                          else
                            Nothing
  where
    check char = BS.elem char vocab
