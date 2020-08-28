{-# LANGUAGE OverloadedStrings #-}

module Lib.Consensus ( fastaToProfileMatrix
           	     )
where

import Data.List

import qualified Data.Vector as V
import qualified Lib.Fasta as F


data ProfileMatrix = ProfileMatrix { lengthPM :: Int
                                   , countA :: V.Vector Int
                                   , countC :: V.Vector Int
                                   , countG :: V.Vector Int
                                   , countT :: V.Vector Int
                                   } deriving Show



initPM :: Int -> ProfileMatrix
initPM n = ProfileMatrix n f f f f where
  f = V.replicate n 0


addPM :: ProfileMatrix -> ProfileMatrix -> ProfileMatrix
addPM pm1 pm2 = ProfileMatrix { countA = f (countA pm1) (countA pm2)
       	                      , countC = f (countC pm1) (countC pm2)
                              , countG = f (countG pm1) (countG pm2)
                              , countT = f (countT pm1) (countT pm2)
                              } where
		  f v1 v2 = V.zipWith (+) v1 v2


fastaToProfileMatrix :: F.Fasta -> ProfileMatrix
fastaToProfileMatrix fasta =
  let
    entries = F.entries fasta
    pm0 = initPM $ length entries
  in
    foldr addPM pm0 entries
