module SumOfMultiples (sumOfMultiples) where
import Data.List (union)

multiplesBelow :: Integer -> Integer -> [Integer]
multiplesBelow _ 0 = []
multiplesBelow limit n = takeWhile (limit >) $ map (n *) [1..]

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit =
  sum $ uniqueMultiples $ map (multiplesBelow limit) factors
  where
    uniqueMultiples = foldl union []
