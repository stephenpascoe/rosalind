{-# LANGUAGE OverloadedStrings #-}

module Problems
    ( Problem
    , Result
    , dna
    , rna
    , revc
    , fib
    , gc
    , hamm
    , iprb
    , prot
    , subs
    ) where

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Builder as B
import Data.Monoid
import Data.Maybe
import Safe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.List
import Control.Monad

import Lib
import Lib.Fasta (parseFasta, mapSeq)
import Lib.Inherit (mateProb)
import qualified Lib.Codon as Codon


-- | A Problem takes an input stream and returns either an error message or an output stream.
--   Streams are represented as lazy bytestrings.
type Result = Either T.Text BS.ByteString
type Problem = BS.ByteString -> Result

dna :: Problem
dna input = case dnaFromByteString input of
  Just seq -> Right (serialise (countBases seq))
  Nothing ->  Left "Not a DNA sequence"
  where
    serialise counts = builderToBS $ B.integerDec (aCount counts) <> " " <>
                                                           B.integerDec (cCount counts) <> " " <>
                                                           B.integerDec (gCount counts) <> " " <>
                                                           B.integerDec (tCount counts)

rna :: Problem
rna input = case dnaFromByteString input of
  Just seq -> Right $ toByteString $ transcribe seq
  Nothing  -> Left "Not a DNA sequence"

revc :: Problem
revc input = case dnaFromByteString input of
  Just seq -> Right $ BS.reverse . toByteString . complement $ seq
  Nothing  -> Left "Not a DNA sequence"

fib :: Problem
fib input = case resultMay of
              Just pairs -> Right $ builderToBS . B.intDec $ pairs
              Nothing -> Left "Parse error"
  where
    args = BS.words input
    resultMay = do
      nStr <- headMay args
      kStr <- atMay args 1
      (n, _) <- BS.readInt nStr
      (k, _) <- BS.readInt kStr
      return $ (rabbitPairs k) !! (n - 1)


gc :: Problem
gc input = case parseFasta input of
  Right fasta -> let
    cmp' (l1, v1) (l2, v2) = compare v1 v2
    (label, maxGC) = maximumBy cmp' (mapSeq gcContent fasta)
    in
      Right $ BS.concat [TE.encodeUtf8 label, "\n", BS.pack $ show (maxGC * 100)]

  Left err -> Left $ T.pack err

hamm :: Problem
hamm input = do
   let lines = BS.lines input
       distanceMay = do
         seqStr1 <- headMay lines
         seqStr2 <- headMay <=< tailMay $ lines
         seq1 <- dnaFromByteString seqStr1
         seq2 <- dnaFromByteString seqStr2
         return $ hammingDistance seq1 seq2
   case distanceMay of
     Just result -> Right $ BS.pack $ show result
     Nothing     -> Left "Parser error"

iprb :: Problem
iprb input = do
  let args = BS.words input
  inStrs <- if (length args) < 3 then
              Left "Not enough inputs"
            else
              Right $ take 3 args
  k <- getInt (inStrs !! 0)
  m <- getInt (inStrs !! 1)
  n <- getInt (inStrs !! 2)
  return $ BS.pack $ show (mateProb k m n)
    where
      getInt str = case BS.readInt str of
                     Just (x, _) -> Right x
                     Nothing     -> Left "Parse Error"

prot :: Problem
prot input =
  let lines = BS.lines input
      protMay = do
        rnaStr <- headMay lines
        rna <- rnaFromByteString rnaStr
        return $ Codon.translate rna

  in case protMay of
    Just prot -> Right $ toByteString prot
    Nothing   -> Left "No translation found"

subs :: Problem
subs input = do  
  let lines = BS.lines input
  dnaS <- maybe (Left "No string found") Right $ headMay lines
  dnaT <- maybe (Left "No string found") Right $ headMay <=< tailMay $ lines
  return $ BS.intercalate " " $ map (BS.pack . show) $ subs1 dnaS dnaT
    where
      subs1 a b = map fst $ filter findPrefix $ zip [1..] (BS.tails a)
        where findPrefix (i, a') = BS.isPrefixOf b a'


-- Utilities

builderToBS = BSL.toStrict . B.toLazyByteString
