--
-- | Day 10
--
module Day10 where

import Relude
import qualified Relude.Unsafe as U (head)
import Data.List (maximum)
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.HashMap.Lazy as H

day10 :: IO ()
day10 = do
  map <- fmap T.unpack . lines <$> readFileText "./input/day10.txt" -- Read input lines
  let (width, height) = (length . U.head $ map, length map) -- Calculate map dimensions

      isAsteroid :: [Int] -> Bool -- Does the target location contain an asteroid
      isAsteroid [y, x] | Just l <- map !!? y, Just '#' <- l !!? x = True | otherwise = False

      asteroids :: [[Int]] -- List of all asteroid locations
      asteroids = filter isAsteroid (sequence [[0..height-1],[0..width-1]])

      path :: [Int] -> [Int] -> [Int] -> [[Int]] -- Generate path between two locations
      path [dy, dx] start e@[ey, ex] = takeWhile (e /=) $ iterate (\[y, x] -> [y+dy, x+dx]) start

      los :: [Int] -> [Int] -> [[Int]] -- Calculate the line of sight between two locations
      los p@[py, px] e@[ey, ex] = let (dy, dx) = (ey - py, ex - px)
                                      gcd' = gcd dy dx
                                      step | gcd' > 0 = [dy `div` gcd', dx `div` gcd'] | otherwise = [dy, dx]
                                      los' | (self:rest) <- filter isAsteroid (path step p e) = rest | otherwise = []
                                  in los'

      pairs :: [([Int], Int)] -- Find paths between all pairs
      pairs = (\[f, t] -> (f, if not (null (los f t)) then 0 else 1)) <$> filter (\[x, y] -> x /= y) (sequence [asteroids, asteroids])

      -- Find the location and visibility count of asteroid with maximum line of sight to all others
      (station, visibility) = L.maximumBy (compare `on` snd) ((\p -> (p, sum . fmap snd . filter ((p ==) . fst) $ pairs)) <$> asteroids)

      angle :: [Int] -> [Int] -> Float -- Find the angle between two points
      angle [sy, sx] [ay, ax] = atan2 (fromIntegral (ax-sx)) (fromIntegral (ay-sy))

      laser :: [[Int]] -- Generate the asteroid ordering
      laser = L.nub . concat . transpose $ (\a -> los station a <> [a]) <$> sortOn (Down . angle station) (filter (station /=) asteroids)

      Just [y, x] = laser !!? 199 -- Find the 200th asteroid

  print visibility
  print $ 100 * x + y
