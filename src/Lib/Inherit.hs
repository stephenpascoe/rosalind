
{- Modelling inheritance
-}


module Lib.Inherit ( mateProb
                   ) where

import Data.List

-- | An allene is Heterozygous, Homozygous recessive or Homozygous dominant
data Allene = HomDom | HomRec | Het deriving (Show, Eq)

-- | A probability is a float
type Prob = Float


-- | A population is a list of (organism, count) pairs with the total count stored for
--   convenience
data Population a = Population { totalPopulation :: Int
                               , orgamisms :: [(a, Int)]
                               } deriving Show


-- | Table of probabilities that two allenes result in a dominant allene.
--   Based on the Punnett square
punnettProb :: Allene -> Allene -> Prob
punnettProb HomDom _ = 1.0
punnettProb _ HomDom = 1.0
punnettProb HomRec Het = 0.5
punnettProb Het HomRec = 0.5
punnettProb Het Het    = 0.75
punnettProb HomRec HomRec = 0.0



-- | Returns a list of all possible results of removing one from a population
-- takeOne :: Eq a => [(a, Int)] -> [(a, [(a, Int)])]
takeOne :: Eq a => Population a -> [(a, Prob, Population a)]
takeOne (Population total as) = map f as
  where
    f (a, c) = ( a
               , (fromIntegral c) / (fromIntegral total)
               , Population (total - 1) (filter f'' $ foldl' f' [] as)
               )
      where
        f' acc (a', c') = if a == a'
                          then (a, c' - 1) : acc
                          else (a', c') : acc
        f'' (_ , 0) = False
        f'' _       = True


-- | Return a list of all maiting pairs and their probability of selection
takeMatingPair pop = do
  (a1, prob1, pop1) <- takeOne pop
  (a2, prob2, pop2) <- takeOne pop1
  return (a1, a2, prob1 * prob2)


-- | Return the probability of a dominant allene being present for a random mating pair
--   given the populations of different allenes types
--   NOTE : This is a brute force approach but does work.  I've seen a much better
--          solution using the "N choose k" formula
mateProb k m n = sum $ map f (takeMatingPair pop)
  where
    pop = Population (k + m + n) [(HomDom, k), (Het, m), (HomRec, n)]
    f (a1, a2, prob) = (punnettProb a1 a2) * prob
