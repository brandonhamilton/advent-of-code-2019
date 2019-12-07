--
-- | Day 4
--
module Day4 where

import Relude

day4 :: IO ()
day4 = do
  let range = [124075..580769] -- Generate range
      valid = filter (liftA2 (&&) adjacent increasing) (reverse . digits <$> range) -- Filter out invalid numbers in range
  print $ length valid -- Part 1
  print $ length (filter adjacentPair valid) -- Part 2
  where
    digits :: Int -> [Int]
    digits n | n < 10 = [n]
    digits n | (q, r) <- quotRem n 10 = r : digits q

    increasing :: [Int] -> Bool
    increasing = fst . foldl' (\(inc, last) curr -> (inc && curr >= last, curr)) (True, 0)

    adjacent :: [Int] -> Bool
    adjacent = liftA2 (/=) length (length . group)

    adjacentPair :: [Int] -> Bool
    adjacentPair d = let g = group d in length g /= length d && any ((== 2) . length) g
