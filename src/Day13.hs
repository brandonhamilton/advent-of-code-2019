--
-- | Day 13
--
module Day13 (day13) where

import Relude
import IntCode
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
    code <- load "./input/day13.txt"
    (bp, map) <- runArcade code ((0, 0), (0, 0))
    print (length (filter (2 ==) (H.elems map)))
    (_, map') <- runArcade (code & ix 0 .~ 2) bp
    print (H.lookupDefault 0 (-1, 0) map')
  where
    boot :: Chan Integer -> Chan Integer -> [Integer] -> IO (MVar ()) -- Boot computer with provided code
    boot input output code = newEmptyMVar >>= (\done -> forkFinally (computer input output 0 0 code) (\_ -> putMVar done ()) >> pure done)

    runArcade :: [Integer] -> BallAndPaddle -> IO (BallAndPaddle, GameState) -- Create and run the arcade
    runArcade code bp = do
      [input, output] <- forM [0..1] $ const newChan
      done <- boot input output (code <> repeat 0)
      bp' <- newMVar bp
      let runJoystick = execStateT (joystick input bp' done) bp
          runScreen = execStateT (screen output bp' done) H.empty
      writeChan input 1
      concurrently runJoystick runScreen
