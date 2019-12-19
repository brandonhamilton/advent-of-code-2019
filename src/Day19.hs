--
-- | Day 19
--
module Day19 (day19) where

import Relude
import IntCode
import Control.Concurrent.Chan

day19 :: IO ()
day19 = do
  code <- load "./input/day19.txt"
  beam <- foldlM (\acc pos -> (+) acc <$> drone code pos) 0 (sequence [[0..49],[0..49]]) -- Part 1
  print beam
  (x, y) <- search code -- Part 2
  print (y + x * 10000)
  where
    drone :: [Integer] -> [Int] -> IO Int
    drone code [x, y] = do
      (input, output, computer) <- boot code -- Boot the computer with the program
      writeChan input x -- Write x position
      writeChan input y -- Write y position
      computer -- Run the program
      readChan output -- Read the result

    search :: [Integer] -> IO (Int, Int)
    search code = go 1800 1900 -- Manually calculated starting point for search based on slope
      where
        go :: Int -> Int -> IO (Int, Int) -- Recursively search for 100x100 area
        go x y = drone code [x, y] >>= \case
                    0 -> go (x+1) y -- Search for valid x position
                    1 -> drone code [x + (100-1), y - (100-1)] >>= \case
                          0 -> go x (y+1) -- Search for valid y position
                          1 -> pure (x, y - (100-1))

