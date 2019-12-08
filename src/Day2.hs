--
-- | Day 2
--
module Day2 where

import Relude
import Control.Lens (ix, (.~), (^?!))
import qualified Data.Text as T
import qualified Data.List as L

day2 :: IO ()
day2 = do
  input <- T.splitOn "," <$> readFileText "./input/day2.txt" -- Read input lines
  let code = fromRight 0 . readEither <$> input -- Convert to Integers
      Just result = runWith code 12 2  -- Interpret program (Part 1)
  print result -- Show result
  let Just [noun, verb] = L.find (\[n, v] -> runWith code n v == Just 19690720) (sequence [[0..99],[0..99]])
  print (100 * noun + verb) -- Show result
  where
    runWith :: [Int] -> Int -> Int -> Maybe Int
    runWith code noun verb = let c = replace 1 noun (replace 2 verb code) in viaNonEmpty head (run c c)

    replace :: Int -> Int -> [Int] -> [Int]
    replace pos val l = l & ix pos .~ val

    op :: (Int -> Int -> Int) -> [Int] -> Int -> Int -> Int
    op f l idx1 idx2 = let get idx = (l ^?! ix idx) in f (get idx1) (get idx2)

    run :: [Int] -> [Int] -> [Int]
    run (99 : cont) code = code
    run (c : i1 : i2 : o : cont) code | c == 1 || c == 2
      = let next = replace o (op (if c == 1 then (+) else (*)) code i1 i2) code
          in run (drop (length code - length cont) next) next
    run (c : cont) code = error $ "Unknown opcode " <> show c <> " at position " <> show (length code - length cont)
