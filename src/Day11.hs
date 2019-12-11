--
-- | Day 11
--
module Day11 where

import Relude
import Data.List (maximum, minimum)
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as H
import Control.Concurrent (forkFinally)
import Control.Concurrent.Chan
import Control.Lens (ix, (.~), (^?!))

data Direction = DUp | DDown | DLeft | DRight deriving (Show)
type Location = (Integer, Integer)
type Hull = H.HashMap Location Integer
type Robot = (Direction, Location, Hull)

compute :: Chan Integer -> Chan Integer -> Int -> Integer -> [Integer] -> IO ()
compute input output ip base code = do
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
          in compute input output (ip + 4) base (set 2 o (get 0 i1 + get 1 i2))
    2 -> let [i1,i2,o] = take 3 (drop (ip + 1) code) -- Multiply
          in compute input output (ip + 4) base (set 2 o (get 0 i1 * get 1 i2))
    3 -> let Just pos = code !!? (ip + 1) -- Input
          in readChan input >>= compute input output (ip + 2) base . set 0 pos
    4 -> let Just pos = code !!? (ip + 1) -- Output
          in writeChan output (get 0 pos) >> compute input output (ip + 2) base code
    5 -> let [b,val] = take 2 (drop (ip + 1) code) -- Jump if True
          in compute input output (if get 0 b > 0 then fromIntegral (get 1 val) else ip + 3) base code
    6 -> let [b,val] = take 2 (drop (ip + 1) code)  -- Jump if False
          in compute input output (if get 0 b == 0 then fromIntegral (get 1 val) else ip + 3) base code
    7 -> let [i1,i2,o] = take 3 (drop (ip + 1) code)  -- Less than
          in compute input output (ip + 4) base (set 2 o (if get 0 i1 < get 1 i2 then 1 else 0))
    8 -> let [i1,i2,o] = take 3 (drop (ip + 1) code) -- Equals
          in compute input output (ip + 4) base (set 2 o (if get 0 i1 == get 1 i2 then 1 else 0))
    9 -> let Just offset = code !!? (ip + 1) -- Adjust relative offset
          in compute input output (ip + 2) (base + get 0 offset) code
    _ -> error $ "Unhandled instruction " <> show op

robot :: Chan Integer -> Chan Integer -> MVar () -> StateT Robot IO ()
robot input output halted = tryReadMVar halted >>= maybe (step >> robot input output halted) pure
  where
    getPanel :: Robot -> Integer -- Read the color of a panel
    getPanel (_, location, hull) = H.lookupDefault 0 location hull

    setPanel :: Integer -> Robot -> Robot -- Write the color of a panel
    setPanel color (d, l, hull) = (d, l, H.insert l color hull)

    move :: Integer -> Robot -> Robot -- Advance the position of the robot
    move 0 (DUp,    (x, y), hull) = (DLeft,  (x-1, y), hull)
    move 0 (DDown,  (x, y), hull) = (DRight, (x+1, y), hull)
    move 0 (DLeft,  (x, y), hull) = (DDown,  (x, y+1), hull)
    move 0 (DRight, (x, y), hull) = (DUp,    (x, y-1), hull)
    move 1 (DUp,    (x, y), hull) = (DRight, (x+1, y), hull)
    move 1 (DDown,  (x, y), hull) = (DLeft,  (x-1, y), hull)
    move 1 (DLeft,  (x, y), hull) = (DUp,    (x, y-1), hull)
    move 1 (DRight, (x, y), hull) = (DDown,  (x, y+1), hull)
    move _ s = s

    step :: StateT Robot IO () -- Process an instruction from the program
    step = do
      (color, direction) <- liftIO $ liftA2 (,) (readChan output) (readChan output) -- Read color and direction
      state <- move direction . setPanel color <$> get -- Update robot state
      put state
      liftIO $ writeChan input (getPanel state) -- Write current panel color

day11 :: IO ()
day11 = do
    program <- T.splitOn "," <$> readFileText "./input/day11.txt" -- Read input lines
    let code = fromRight 0 . readEither <$> program
    part1 <- runRobot 0 code -- Run robot starting on BLACK panel
    print (H.size part1) -- How many panels were painted
    part2 <- runRobot 1 code -- Run robot starting on WHITE panel
    let ls = H.keys part2
        (xs, ys) = (fst <$> ls, snd <$> ls)
    -- Render panels
    forM_ [minimum ys..maximum ys] $ \y -> putTextLn . T.concat $ fmap (\x -> paint (H.lookupDefault 0 (x, y) part2)) [minimum xs..maximum xs]
  where
    paint :: Integer -> Text
    paint 0 = "  " -- White
    paint _ = "██" -- Black

    boot :: Chan Integer -> Chan Integer -> [Integer] -> IO (MVar ()) -- Boot computer with provided code
    boot input output code = newEmptyMVar >>= (\done -> forkFinally (compute input output 0 0 code) (\_ -> writeChan output 0 >> writeChan output 0 >> putMVar done ()) >> pure done)

    runRobot :: Integer -> [Integer] -> IO Hull -- Create and run the robot
    runRobot startColor code = do
      [input, output] <- forM [0..1] $ const newChan
      writeChan input startColor
      robot' <- robot input output <$> boot input output (code <> repeat 0)
      (_, _, hull) <- execStateT robot' (DUp, (0,0), H.empty)
      pure hull
