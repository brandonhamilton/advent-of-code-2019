--
-- | Day 16
--
module Day16 (day16) where

import Relude
import qualified Data.Text as T

day16 :: IO ()
day16 = do
  input <- T.chunksOf 1 <$> readFileText "./input/day16.txt" -- Read input data
  let digits = (fromRight 0 . readEither) <$> input
      part1 input = T.take 8 (foldMap show . fromMaybe [] $ (iterate phase input !!? 100))
      offset = fromRight 0 . readEither $ (T.concat (take 7 input))
      digits' = drop offset $ take (10000 * length digits) (cycle digits)
      part2 input = T.take 8 (foldMap show . fromMaybe [] $ (iterate partialSum input !!? 100))
  putTextLn $ part1 digits
  putTextLn $ part2 digits'
  where
    pattern :: Int -> [Integer] -- Generate pattern for a position
    pattern = drop 1 . cycle . concat . (<$> [0, 1, 0, -1]) . replicate

    phase :: [Integer] -> [Integer] -- Calculate phase
    phase l = fmap ((`mod` 10) . abs . sum . zipWith (*) l . pattern) . take (length l) $ [1..]

    partialSum :: [Integer] -> [Integer]
    partialSum i = go i (sum i)
      where go [] _ = []
            go (x:xs) acc = ((`mod` 10) . abs $ acc) : go xs (acc - x)
