--
-- | Day 1
--
module Day1 where

import Relude

day1 :: IO ()
day1 = do
  input <- lines <$> readFileText "./input/day1.txt" -- Read input lines
  let weights = fromRight 0 . readEither <$> input -- Convert to Integers
  print $ sum (fuel <$> weights) -- Part 1
  print $ sum (recFuel <$> weights) -- Part 2
  where
    fuel :: Int -> Int
    fuel = flip (-) 2 . flip div 3

    recFuel :: Int -> Int
    recFuel x | fuel x < 0 = 0
    recFuel x = let fx = fuel x in fx + recFuel fx
