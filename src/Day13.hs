--
-- | Day 13
--
module Day13 (day13) where

import Relude
import qualified Data.Text as T
import Data.List (maximum, minimum)
import Control.Concurrent (forkFinally, modifyMVar_)
import Control.Concurrent.Async (concurrently)
import Control.Concurrent.Chan
import qualified Data.HashMap.Lazy as H
import Control.Lens (ix, (.~), (^?!))

type Location = (Integer, Integer)
type BallAndPaddle = (Location, Location)
type GameState = (H.HashMap Location Integer)

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

render :: GameState -> IO ()
render map = do
  let ls = H.keys map
      (xs, ys) = (fst <$> ls, snd <$> ls)
  forM_ [minimum ys..maximum ys] $ \y -> putTextLn . T.concat $ fmap (\x -> renderTile (H.lookupDefault 0 (x, y) map)) [minimum xs..maximum xs]
  where
    renderTile :: Integer -> Text
    renderTile 1 = "██"
    renderTile 2 = "▒▒"
    renderTile 3 = "══"
    renderTile 4 = " o"
    renderTile _ = "  "

screen :: Chan Integer -> MVar BallAndPaddle -> MVar () -> StateT GameState IO ()
screen output ballPaddle halted = tryReadMVar halted >>= maybe (step >> screen output ballPaddle halted) pure
  where
    step :: StateT GameState IO () -- Process an instruction from the program
    step = do
      location <- liftIO $ liftA2 (,) (readChan output) (readChan output) -- Read co-ordindates
      tile <- liftIO (readChan output) -- Read tile
      modify $ H.insert location tile -- Save tile
      when (tile == 3) $ liftIO $ modifyMVar_ ballPaddle (\(ball, _) -> pure (ball, location)) -- Update paddle position
      when (tile == 4) $ liftIO $ modifyMVar_ ballPaddle (\(_, paddle) -> pure (location, paddle)) -- Update ball position

joystick :: Chan Integer -> MVar BallAndPaddle -> MVar () -> StateT BallAndPaddle IO ()
joystick input ballPaddle halted = tryReadMVar halted >>= maybe (step >> joystick input ballPaddle halted) pure
  where
    move :: Integer -> Integer -> Integer
    move ix px | ix < px = -1 | ix > px = 1 | otherwise = 0

    step :: StateT BallAndPaddle IO () -- Process an instruction from the program
    step = do
      (b, _) <- get
      bp@(b', p) <- liftIO $ readMVar ballPaddle
      when (b /= b') $ do
        liftIO $ writeChan input (move (fst b') (fst p))
        put bp

day13 :: IO ()
day13 = do
    program <- T.splitOn "," <$> readFileText "./input/day13.txt" -- Read input lines
    let code = fromRight 0 . readEither <$> program
    (bp, map) <- runArcade code ((0, 0), (0, 0))
    print (length (filter (2 ==) (H.elems map)))
    (_, map') <- runArcade (code & ix 0 .~ 2) bp
    print (H.lookupDefault 0 (-1, 0) map')
  where
    boot :: Chan Integer -> Chan Integer -> [Integer] -> IO (MVar ()) -- Boot computer with provided code
    boot input output code = newEmptyMVar >>= (\done -> forkFinally (compute input output 0 0 code) (\_ -> putMVar done ()) >> pure done)

    runArcade :: [Integer] -> BallAndPaddle -> IO (BallAndPaddle, GameState) -- Create and run the arcade
    runArcade code bp = do
      [input, output] <- forM [0..1] $ const newChan
      done <- boot input output (code <> repeat 0)
      bp' <- newMVar bp
      let runJoystick = execStateT (joystick input bp' done) bp
          runScreen = execStateT (screen output bp' done) H.empty
      writeChan input 1
      concurrently runJoystick runScreen
