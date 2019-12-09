--
-- | Day 9
--
module Day9 where

import Relude
import qualified Data.Text as T
import Control.Lens (ix, (.~), (^?!))

day9 :: IO ()
day9 = do
  program <- T.splitOn "," <$> readFileText "./input/day9.txt" -- Read input lines
  let code = fromRight 0 . readEither <$> program
  compute [1] 0 0 (code <> repeat 0) -- Part 1
  compute [2] 0 0 (code <> repeat 0) -- Part 2
  where
    compute :: [Integer] -> Int -> Integer -> [Integer] -> IO ()
    compute input ip base code = do
      let Just instr = code !!? ip
          digits n | n < 10 = [n] | (q, r) <- n `quotRem` 10 = r : digits q
          (mode, op) = first digits (instr `quotRem` 100)
          get p val | Just 1 == (mode !!? p) = val -- Immediate mode
                    | Just 2 == (mode !!? p) = code ^?! ix (fromIntegral (base + val)) -- Relative position mode
                    | otherwise = code ^?! ix (fromIntegral val) -- Absolute position mode
          set p pos val | Just 2 == (mode !!? p) =  code & ix (fromIntegral (base + pos)) .~ val -- Write value to relative position
                        | otherwise = code & ix (fromIntegral pos) .~ val -- Write value to absolute position
      case op of
        99 -> pure () -- Halt
        1 -> let [i1,i2,o] = take 3 (drop (ip + 1) code) -- Add
              in compute input (ip + 4) base (set 2 o (get 0 i1 + get 1 i2))
        2 -> let [i1,i2,o] = take 3 (drop (ip + 1) code) -- Multiply
              in compute input (ip + 4) base (set 2 o (get 0 i1 * get 1 i2))
        3 -> let Just pos = code !!? (ip + 1) -- Input
              in let val:rest = input in compute rest (ip + 2) base (set 0 pos val)
        4 -> let Just pos = code !!? (ip + 1) -- Output
              in print (get 0 pos) >> compute input (ip + 2) base code
        5 -> let [b,val] = take 2 (drop (ip + 1) code) -- Jump if True
              in compute input (if get 0 b > 0 then fromIntegral (get 1 val) else ip + 3) base code
        6 -> let [b,val] = take 2 (drop (ip + 1) code)  -- Jump if False
              in compute input (if get 0 b == 0 then fromIntegral (get 1 val) else ip + 3) base code
        7 -> let [i1,i2,o] = take 3 (drop (ip + 1) code)  -- Less than
              in compute input (ip + 4) base (set 2 o (if get 0 i1 < get 1 i2 then 1 else 0))
        8 -> let [i1,i2,o] = take 3 (drop (ip + 1) code) -- Equals
              in compute input (ip + 4) base (set 2 o (if get 0 i1 == get 1 i2 then 1 else 0))
        9 -> let Just offset = code !!? (ip + 1) -- Adjust relative offset
              in compute input (ip + 2) (base + get 0 offset) code
        _ -> error $ "Unhandled instruction " <> show op
