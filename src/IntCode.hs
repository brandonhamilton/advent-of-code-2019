module IntCode (load, boot, computer) where

import Relude
import qualified Data.Text as T
import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
import Control.Lens (ix, (.~), (^?!))

-- | Load an Incode program from a file
load :: FilePath -> IO [Integer]
load codePath = do
  program <- T.splitOn "," <$> readFileText codePath -- Read input lines
  let code = fromRight 0 . readEither <$> program -- Convert to integers
  pure code

-- | Create a computer and initialize it with an IntCode program
boot :: [Integer] -> IO (Chan Integer, Chan Integer, IO ())
boot code = do
  [input, output] <- forM [0..1] $ const newChan
  pure (input, output, computer input output 0 0 (code <> repeat 0))

--  | An IntCode computer
computer :: Chan Integer -> Chan Integer -> Int -> Integer -> [Integer] -> IO ()
computer input output ip base code = do
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
          in computer input output (ip + 4) base (set 2 o (get 0 i1 + get 1 i2))
    2 -> let [i1,i2,o] = take 3 (drop (ip + 1) code) -- Multiply
          in computer input output (ip + 4) base (set 2 o (get 0 i1 * get 1 i2))
    3 -> let Just pos = code !!? (ip + 1) -- Input
          in readChan input >>= computer input output (ip + 2) base . set 0 pos . fromIntegral
    4 -> let Just pos = code !!? (ip + 1) -- Output
          in writeChan output (fromIntegral (get 0 pos)) >> computer input output (ip + 2) base code
    5 -> let [b,val] = take 2 (drop (ip + 1) code) -- Jump if True
          in computer input output (if get 0 b > 0 then fromIntegral (get 1 val) else ip + 3) base code
    6 -> let [b,val] = take 2 (drop (ip + 1) code)  -- Jump if False
          in computer input output (if get 0 b == 0 then fromIntegral (get 1 val) else ip + 3) base code
    7 -> let [i1,i2,o] = take 3 (drop (ip + 1) code)  -- Less than
          in computer input output (ip + 4) base (set 2 o (if get 0 i1 < get 1 i2 then 1 else 0))
    8 -> let [i1,i2,o] = take 3 (drop (ip + 1) code) -- Equals
          in computer input output (ip + 4) base (set 2 o (if get 0 i1 == get 1 i2 then 1 else 0))
    9 -> let Just offset = code !!? (ip + 1) -- Adjust relative offset
          in computer input output (ip + 2) (base + get 0 offset) code
    _ -> error $ "Unhandled instruction " <> show op
