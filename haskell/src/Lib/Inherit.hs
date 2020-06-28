
{- Modelling inheritance
-}


module Lib.Inherit ( mateProb
                   ) where


import Math.Combinatorics.Exact.Binomial


-- | Return the probability of a dominant allene being present for a random mating pair
--   given the populations of different allenes types
mateProb :: Int -> Int -> Int -> Float
mateProb k m n = 1.0 - (rr + hh * 0.25 + hr * 0.5)
  where
    total = k + m + n
    rr = fromIntegral (n `choose` 2) / fromIntegral (total `choose` 2)
    hh = fromIntegral (m `choose` 2) / fromIntegral (total `choose` 2)
    hr = fromIntegral ((n `choose` 1) * (m `choose` 1)) / fromIntegral (total `choose` 2)
