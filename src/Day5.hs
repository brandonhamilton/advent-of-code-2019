--
-- | Day 5
--
module Day5 where

import Relude
import Control.Lens (ix, (.~), (^?!))
import qualified Data.Text as T

day5 :: IO ()
day5 = do
  program <- T.splitOn "," <$> readFileText "./input/day5.txt" -- Read input lines
  let code = fromRight 0 . readEither <$> program
  compute 1 0 code
  compute 5 0 code
  where
    compute :: Int -> Int -> [Int] -> IO ()
    compute input ip code = do
      let Just instr = code !!? ip
          digits n | n < 10 = [n] | (q, r) <- n `quotRem` 10 = r : digits q
          (mode, op) = first digits (instr `quotRem` 100)
          param pos val | Just 1 == (mode !!? pos) = val | otherwise = code ^?! ix val
      case op of
        99 -> pure () -- Halt
        1 -> let [i1,i2,o] = take 3 (drop (ip + 1) code) -- Add
              in compute input (ip + 4) (code & ix o .~ (+) (param 0 i1) (param 1 i2))
        2 -> let [i1,i2,o] = take 3 (drop (ip + 1) code) -- Multiply
              in compute input (ip + 4) (code & ix o .~ (*) (param 0 i1) (param 1 i2))
        3 -> let Just pos = code !!? (ip + 1) -- Input
              in compute input (ip + 2) (code & ix pos .~ input)
        4 -> let Just pos = code !!? (ip + 1) -- Output
              in print (param 0 pos) >> compute input (ip + 2) code
        5 -> let [b,val] = take 2 (drop (ip + 1) code) -- Jump if True
              in compute input (if param 0 b > 0 then param 1 val else ip + 3) code
        6 -> let [b,val] = take 2 (drop (ip + 1) code)  -- Jump if False
              in compute input (if param 0 b == 0 then param 1 val else ip + 3) code
        7 -> let [i1,i2,o] = take 3 (drop (ip + 1) code)  -- Less than
              in compute input (ip + 4) (code & ix o .~ if param 0 i1 < param 1 i2 then 1 else 0)
        8 -> let [i1,i2,o] = take 3 (drop (ip + 1) code) -- Equals
              in compute input (ip + 4) (code & ix o .~ if param 0 i1 == param 1 i2 then 1 else 0)
        _ -> error $ "Unhandled instruction " <> show op
