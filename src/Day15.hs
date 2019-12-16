{-# LANGUAGE RecordWildCards #-}
--
-- | Day 15
--
module Day15 (day15) where

import Relude
import IntCode
import Data.List (maximum, minimum)
import qualified Data.Text as T (concat)
import Control.Concurrent.Async (race)
import Control.Concurrent.Chan
import qualified Data.HashMap.Lazy as H

type Location = (Int, Int)
data Droid = Droid
  { location :: Location
  -- ^ Current location of the droid
  , orientation :: Int
  -- ^ Current orientation of the droid
  , level :: H.HashMap Location Int
  -- ^ Level map of explored tiles
  } deriving (Show)

render :: Droid -> IO () -- Render the known level map
render Droid{..} = do
  let lvl = H.insert location 3 level
      ls = H.keys lvl
      (xs, ys) = (fst <$> ls, snd <$> ls)
  putText "\ESC[2J"
  forM_ [minimum ys..maximum ys] $ \y -> putTextLn . T.concat $ fmap (\x -> renderTile (H.lookupDefault 5 (x, y) lvl)) [minimum xs..maximum xs]
  where
    renderTile :: Int -> Text
    renderTile 0 = "██" -- Wall
    renderTile 1 = "░░" -- Floor
    renderTile 2 = "≡≡" -- Oxygen
    renderTile 3 = "«»" -- Droid
    renderTile 4 = "¤¤" -- Origin
    renderTile _ = "  " -- Unknown

droid :: Chan Integer -> Chan Integer -> IO Droid -- The droid logic
droid input output = execStateT go (Droid (0,0) 1 (H.singleton (0,0) 4))
  where
    forward :: Location -> Int -> Location -- Next location in the desired direction
    forward (x, y) d | d == 1 = (x, y-1) | d == 2 = (x, y+1) | d == 3 = (x-1, y) | d == 4 = (x+1, y)

    next :: [Int] -> Droid -> Int -- Pick a valid direction to move forward
    next dirs dr@(Droid l d lvl) = let proceed to = 0 /= H.lookupDefault 1 to lvl in fromMaybe 0 $ find (proceed . forward l) dirs

    direction :: Int -> [Int] -- Preferences of directions to move
    direction d | d == 1 = [4,1,3,2] | d == 2 = [3,2,4,1] | d == 3 = [1,3,2,4] | d == 4 = [2,4,1,3]

    update :: Int -> Int -> Droid -> Droid -- Update the droid state based on the outcome of the move
    update to 0 d@(Droid loc _ lvl) = let loc' = forward loc to in d{ level = H.insert loc' 0 lvl }
    update to status d@(Droid loc _ lvl) = let loc' = forward loc to in d{ location = loc', orientation = to, level = H.insert loc' status lvl }

    go :: StateT Droid IO ()
    go = do
      Droid loc d lvl <- get -- Get the current droid state
      move <- gets . next . direction $ d -- Calculate the next direction
      liftIO $ writeChan input (fromIntegral move) -- Write direction
      status <- liftIO $ readChan output -- Read status
      modify (update move (fromIntegral status)) -- Update the droid state
      if Just 4 == H.lookup (forward loc move) lvl then pure () else go -- Continue until the whole map has been explored

paths :: Location -> H.HashMap Location Int -> (H.HashMap Location Int, Int)
paths from lvl = shortest edges (H.singleton from 0) 0
  where
    nodes = H.keys . H.filter (/= 0) $ lvl -- Find valid locations on the map
    adjacent [(x,y), l] = l == (x, y-1) || l == (x, y+1) || l == (x-1, y) || l == (x+1, y)
    edges = filter adjacent (sequence [nodes, nodes]) -- Convert map into edge graph

    shortest :: [[Location]] -> H.HashMap Location Int -> Int -> (H.HashMap Location Int, Int)
    shortest edgs seen count -- Dijkstra's shorted path algorithm
      | null scout = (seen, count) -- No futher edges to explore
      | otherwise = shortest edgs explore (count + 1) -- Explore the new locations
      where
        scout = filter (\[f, t] -> f `H.member` seen && not (t `H.member` seen)) edgs
        explore = H.union seen $ H.fromList [(t, d + 1) | [f, t] <- scout, let d = seen H.! f]

day15 :: IO ()
day15 = do
  Droid _ _ lvl <- runDroid -- Run the repair droid
  let (fromOrigin, _) = paths (0,0) lvl -- Paths from origin to all other locations
      Just oxygen = viaNonEmpty head . H.keys . H.filter (== 2) $ lvl -- Location of the oxygen
  print (fromOrigin H.! oxygen) -- Path from the origin to the oxygen
  let (_, pathCount) = paths oxygen lvl -- Total path count from the oxygen to all locations
  print pathCount
  where
    runDroid :: IO Droid -- Create and run the repair droid
    runDroid = do
      (input, output, computer) <- boot =<< load "./input/day15.txt"
      Right r <- race computer (droid input output)
      pure r
