{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitPrelude #-}

module Lib.Fasta ( parseFasta
                 , mapSeq
                 , Entry(..)
                 , Fasta
                 , entries
                 )
where

import Prelude hiding (takeWhile, seq)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.ByteString (Parser, Result, parseOnly)
import Control.Applicative

import Lib.Sequence

-- | For the moment we model the contents of a Fasta file as a list of pairs

data Entry = Entry T.Text Sequence deriving Show
newtype Fasta = Fasta {entries :: [Entry]} deriving Show


-- Parsing FASTA files

parseFasta :: BS.ByteString -> Either String Fasta
parseFasta input = parseOnly fasta input

fasta :: Parser Fasta
fasta = Fasta <$> entry `sepBy` eol

entry :: Parser Entry
entry = Entry <$> label <*> seq

seq = do
  seqStr <- BS.concat <$> seqLine `sepBy` eol
  -- TODO : This needs improving.  Some sequences could be either DNA or RNA
  let seqMay = dnaFromByteString seqStr <|> rnaFromByteString seqStr
  case seqMay of
    Just seq -> return seq
    Nothing  -> fail "Unrecognised sequence"


label :: Parser T.Text
label = do
  char '>'
  result <- takeTill isEol
  return $ TE.decodeUtf8 result

seqLine = do
  next <- peekChar
  case next of
    Just '>' -> fail "label"
    Just _   -> takeTill isEol
    Nothing  -> ""

eol :: Parser ()
eol = string "\n" *> pure ()

isEol = inClass "\n"



-- | Operations on FASTA

-- | Map over entries returning a list of pairs of (label, result)
mapSeq :: (Sequence -> a) -> Fasta -> [(T.Text, a)]
mapSeq f (Fasta entries) = fmap f' entries
  where
    f' entry@(Entry label seq) = (label, f seq)
