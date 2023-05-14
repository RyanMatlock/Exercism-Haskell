module CollatzConjecture (collatz) where

import Data.Maybe (fromMaybe)

collatz :: Integer -> Maybe Integer
collatz n
  | n < 1 = Nothing
  | n == 1 = Just 0
  | mod n 2 == 0 = Just $ succ $ fromMaybe 0 $ collatz $ n `div` 2
  | otherwise = Just $ succ $ fromMaybe 0 $ collatz $ (3 * n) + 1
