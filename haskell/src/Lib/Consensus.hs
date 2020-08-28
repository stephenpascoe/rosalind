{-# LANGUAGE OverloadedStrings #-}

module Consensus ( fastaToProfileMatrix
                 )
where

import Data.List

import qualified Vector as V
import qualified Lib.Fasta as F


data ProfileMatrix = ProfileMatrix { lengthPM :: int
                                   , countA :: V.Vector Int
                                   , countC :: V.Vector Int
                                   , countG :: V.Vector Int
                                   , countT :: V.Vector Int
                                   } deriving Show



initPM :: Int -> ProfileMatrix
initPM n = ProfileMatrix n f f f f where
  f = V.replicate n 0


addPM :: ProfileMatrix -> ProfileMatrix -> Either String ProfileMatrix
addPM pm1 pm2 =
  let
    addPM' = ProfileMatrix { countA = f (countA pm1) (countA pm2)
                           , countC = f (countC pm1) (countC pm2)
                           , countG = f (countG pm1) (countG pm2)
                           , countT = f (countT pm1) (countT pm2)
                           }
    f v1 v2 = V zipWith (+) v1 v2
  in
    case (lengthPM pm1) /= (lengthPM pm2) of
      True -> Left "ProfileMatrixes are not the same length"
      False -> Right addPM' pm1 pm2


fastaToProfileMatrix :: F.Fasta -> ProfileMatrix
fastaToProfileMatrix fasta =
  let
    entries = F.entries fasta
    pm0 = intPM $ length entries
  in
    foldr addPM pm0 entries
