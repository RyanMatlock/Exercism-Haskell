module SumOfMultiples (sumOfMultiples) where

import Data.List (union)

multiplesBelow :: Integer -> Integer -> [Integer]
multiplesBelow _ 0 = [0]
multiplesBelow limit n = takeWhile (\x -> x < limit) $ map ((*) n) [1..]

uniqueMultiples :: [[Integer]] -> [Integer]
uniqueMultiples nss = foldl union [] nss

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit =
  sum $ uniqueMultiples $ map (multiplesBelow limit) factors
