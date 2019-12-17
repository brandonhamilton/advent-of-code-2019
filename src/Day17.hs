--
-- | Day 17
--
module Day17 (day17) where

import Relude
import IntCode
import Control.Concurrent.Async (withAsync, wait, race, concurrently)
import Control.Concurrent.Chan
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.HashMap.Lazy as H
import Control.Lens (ix, (.~))

type Area = H.HashMap (Int, Int) Text

camera :: Chan Integer -> MVar () -> Text -> IO Text -- Camera logic
camera output done scaffold = let get = (T.snoc scaffold . chr . fromIntegral <$> readChan output)
                              in tryReadMVar done >>= maybe (get >>= camera output done) (const (pure scaffold))

robot :: Chan Integer -> IO Integer -- The robot logic
robot output = readChan output >>= \out -> if out < 128 then robot output else pure out

path :: (Int, Int) -> Text -> Area -> Text
path location orientation area = go location orientation ""
  where
    go :: (Int, Int) -> Text -> Text -> Text
    go l@(x, y) dir instr' | forward `H.member` area = go forward dir (instr' <> "F") -- Move forward
                           | fst left `H.member` area = go l (snd left) (instr' <> "L") -- Turn left
                           | fst right `H.member` area = go l (snd right) (instr' <> "R") -- Turn right
                           | otherwise = instr' -- End
      where
        forward | dir == "^" = (x, y-1) | dir == "<" = (x-1, y) | dir == "V" = (x, y+1) | dir == ">" = (x+1, y)
        left | dir == "^" = ((x-1, y), "<") | dir == "<" = ((x, y+1), "V") | dir == "V" = ((x+1, y),">") | dir == ">" = ((x, y-1), "^")
        right | dir == "^" = ((x+1, y), ">") | dir == "<" = ((x, y-1), "^") | dir == "V" = ((x-1, y), "<") | dir == ">" = ((x, y+1), "V")

day17 :: IO ()
day17 = do
  code <- load "./input/day17.txt"
  camera <- runCamera code -- Get image from camera
  let maze = concat $ zipWith (\row cols -> (\(col, ch) -> ((col, row), ch)) <$> cols) [0..] (zip [0..] <$> (T.chunksOf 1 <$> lines (T.strip camera)))
      maze' = H.fromList maze
      adjacent (x, y) = [(x, y-1), (x, y+1), (x-1, y), (x+1, y)] -- Adjacent nodes to a location
      nodes :: Area = H.filter (== "#") maze' -- Find valid locations on the map
      intersections = filter (all (`H.member` nodes) . adjacent) (H.keys nodes) -- Find all intersections
  print (sum (uncurry (*) <$> intersections)) -- Part 1
  let [(location, orientation)] = H.toList . H.filter (`notElem` [".", "#"]) $ maze' -- Find the robot start position and orientation
      rle f | f == "L" = "L" | f == "R" = "R" | otherwise = show . T.length $ f -- Encoding
      p = T.intercalate "," (rle <$> T.group (path location orientation nodes)) -- Calculate the path over the scaffolding
      input' = "A,B,A,C,A,C,B,C,C,B\nL,4,L,4,L,10,R,4\nR,4,L,4,L,4,R,8,R,10\nR,4,L,10,R,10\nn\n" -- Manually calculated :(
  dust <- runRobot code input' -- Send the robot input instructions
  print dust -- Part 2
  where
    runCamera :: [Integer] -> IO Text -- Run the initial camera feed to determine the map
    runCamera code = do
      (input, output, computer) <- boot code
      halted <- newEmptyMVar
      withAsync computer $ \computer' -> -- Run the program on the computer
        withAsync (camera output halted "") $ \camera' -> do -- Run the camera feed
          wait computer' -- Wait for the computer to halt
          putMVar halted () -- Indicate that the computer is finished
          writeChan output 10
          wait camera' -- Get the value from the camera

    runRobot :: [Integer] -> Text -> IO Integer -- Run the robot
    runRobot code input' = do
      (input, output, computer) <- boot (code & ix 0 .~ 2) -- Run the modified program on the computer
      mapM_ (writeChan input . fromIntegral . ord) (T.unpack input') -- Write all input to the channel
      snd <$> concurrently computer (robot output) -- Run the computer and return the output
