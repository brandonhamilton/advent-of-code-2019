module Solutions where

import Relude
import Lens.Micro (ix, (.~), (^?!))
import qualified Data.Text as T

day1 :: IO ()
day1 = do
  input <- lines <$> readFileText "./input/day1.txt" -- Read input lines
  let weights = fromRight 0 . readEither <$> input -- Convert to Integers
  putTextLn . show $ sum (fuel <$> weights) -- Part 1
  putTextLn . show $ sum (recFuel <$> weights) -- Part 2
  where
    fuel :: Integer -> Integer
    fuel = flip (-) 2 . flip div 3

    recFuel :: Integer -> Integer
    recFuel x | fuel x < 0 = 0
    recFuel x = let fx = fuel x in fx + recFuel fx
