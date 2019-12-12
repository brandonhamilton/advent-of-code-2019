--
-- | Day 12
--
module Day12 where

import Relude
import Relude.Unsafe (fromJust)
import Data.Char (isDigit)
import qualified Data.Text as T

day12 :: IO ()
day12 = do
  input <- lines <$> readFileText "./input/day12.txt" -- Read input data
  let moons = parse <$> input -- Parse positions
      Just (_, _, energy') = simulation moons !!? 1000 -- Calculate the energy of step 1000
      Just cx = cycleStep $ simulation (onlyDimension 0 <$> moons) -- Simulate each dimension separately
      Just cy = cycleStep $ simulation (onlyDimension 1 <$> moons) --   and find the index where a half-cycle
      Just cz = cycleStep $ simulation (onlyDimension 2 <$> moons) --   occurs for each dimension
      cycleAt = lcm (2*cx) (lcm (2*cy) (2*cz)) -- Calculate the first full-cycle across all dimensions
  print energy' -- Part 1
  print cycleAt -- Part 2
  where
    parse :: Text -> [Int] -- Parse the input into positions
    parse t = fromRight 0 . readEither <$> T.splitOn "," (T.filter (\c -> isDigit c || (c == ',') || (c == '-')) t)

    simulation :: [[Int]] -> [([[Int]], [[Int]], Int)] -- Run a full simulation
    simulation m = iterate step (m, (0 <$) <$> m, 0)

    step :: ([[Int]], [[Int]], Int) -> ([[Int]], [[Int]], Int) -- Perform a single step of the simulation
    step (moons, velocities, _) =
      let pairs = (\m -> (m, filter (/= m) moons)) <$> moons -- Create a list of pairs of moons
          delta = foldl' (zipWith (+)) [0,0,0] <$> (velocity <$> pairs) -- Calculate velocity delta
          velocities' = zipWith (zipWith (+)) velocities delta -- Update all velocities
          moons' = zipWith (zipWith (+)) moons velocities' -- Update all positions
          energy' = sum ((\(m, v) -> energy m * energy v) <$> zip moons' velocities') -- Calculate total energy
      in (moons', velocities', energy')

    gravity :: Int -> Int -> Int -- Calculate velocity change due to gravity
    gravity x x' | x == x' = 0 | x < x' = 1 | x > x' = -1

    velocity :: ([Int], [[Int]]) -> [[Int]] -- Calculate affect of moons on each other
    velocity (moon, others) = zipWith gravity moon <$> others

    energy :: [Int] -> Int -- Calculate the energy over dimensions
    energy = sum . (abs <$>)

    cycleStep :: [([[Int]], [[Int]], Int)] -> Maybe Int -- Detect which step a half-cycle occurs on
    cycleStep sim = find (isHalfCycle . (sim !!?)) [1..]

    onlyDimension :: Int -> [Int] -> [Int]
    onlyDimension dim lst = [fromJust (lst !!? dim)]

    isHalfCycle :: Maybe ([[Int]], [[Int]], Int) -> Bool -- Determine half-cycle by checking for zero velocity
    isHalfCycle (Just (_, _, 0)) = True
    isHalfCycle _ = False
