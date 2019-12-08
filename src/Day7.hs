--
-- | Day 7
--
module Day7 where

import Relude
import Data.List (maximum)
import Control.Concurrent (forkFinally)
import Control.Concurrent.Chan
import Control.Lens (ix, (.~), (^?!))
import qualified Data.Text as T

day7 :: IO ()
day7 = do
  program <- T.splitOn "," <$> readFileText "./input/day7.txt" -- Read input lines
  let code = fromRight 0 . readEither <$> program
  maxThrust <- maximum <$> mapM (runSystem code) (permutations [0..4]) -- Part 1
  print maxThrust
  maxThrust' <- maximum <$> mapM (runSystem code) (permutations [5..9]) -- Part 2
  print maxThrust'
  where
    runSystem :: [Int] -> [Int] -> IO Int
    runSystem code phases = do
      outputs <- mapM (\phase -> newChan >>= \chan -> writeChan chan phase >> pure chan) phases -- Create output wires
      let Just thruster = outputs !!? 4 -- Thruster is the final output
      writeChan thruster 0 -- Initiate the process
      let configure (input, amps) output = (output, amplifier input output : amps) -- Configre the amplifiers
      amps <- mapM ($ code) (snd . foldl' configure (thruster, []) $ outputs) -- Run the program on all amplifiers
      mapM_ takeMVar amps -- Wait for all amplifiers to halt
      readChan thruster -- Read the thruster output

    amplifier :: Chan Int -> Chan Int -> [Int] -> IO (MVar ())
    amplifier input output code = newEmptyMVar >>= (\done -> forkFinally (compute input output 0 code) (\_ -> putMVar done ()) >> pure done)

    compute :: Chan Int -> Chan Int -> Int -> [Int] -> IO ()
    compute input output ip code = do
      let Just instr = code !!? ip
          digits n | n < 10 = [n] | (q, r) <- n `quotRem` 10 = r : digits q
          (mode, op) = first digits (instr `quotRem` 100)
          param pos val | Just 1 == (mode !!? pos) = val | otherwise = code ^?! ix val
      case op of
        99 -> pure () -- Halt
        1 -> let [i1,i2,o] = take 3 (drop (ip + 1) code) -- Add
              in compute input output (ip + 4) (code & ix o .~ param 0 i1 + param 1 i2)
        2 -> let [i1,i2,o] = take 3 (drop (ip + 1) code) -- Multiply
              in compute input output (ip + 4) (code & ix o .~ param 0 i1 * param 1 i2)
        3 -> let Just pos = code !!? (ip + 1) -- Input
              in readChan input >>= \val -> compute input output (ip + 2) (code & ix pos .~ val)
        4 -> let Just pos = code !!? (ip + 1) -- Output
              in writeChan output (param 0 pos) >> compute input output (ip + 2) code
        5 -> let [b,val] = take 2 (drop (ip + 1) code) -- Jump if True
              in compute input output (if param 0 b > 0 then param 1 val else ip + 3) code
        6 -> let [b,val] = take 2 (drop (ip + 1) code)  -- Jump if False
              in compute input output (if param 0 b == 0 then param 1 val else ip + 3) code
        7 -> let [i1,i2,o] = take 3 (drop (ip + 1) code)  -- Less than
              in compute input output (ip + 4) (code & ix o .~ if param 0 i1 < param 1 i2 then 1 else 0)
        8 -> let [i1,i2,o] = take 3 (drop (ip + 1) code) -- Equals
              in compute input output (ip + 4) (code & ix o .~ if param 0 i1 == param 1 i2 then 1 else 0)
        _ -> error $ "Unhandled instruction " <> show op
