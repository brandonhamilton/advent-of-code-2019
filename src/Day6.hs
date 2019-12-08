--
-- | Day 6
--
module Day6 where

import Relude
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.HashMap.Lazy as H

day6 :: IO ()
day6 = do
  input <- lines <$> readFileText "./input/day6.txt" -- Read input lines
  let orbits = (\[i, o] -> (o, i)) . T.splitOn ")" <$> input -- Extract all orbits
      orbitMap = H.fromList orbits -- Create a mapping of orbits
      len m l (k, v) | Just i <- H.lookup v m = l + 1 + len m 0 (v, i) | otherwise = l + 1 -- Recursively calculate length
      path m p k | Just i <- H.lookup k m = p <> [i] <> (path m [] i) | otherwise = p -- Recursively calculate path
  print $ foldl' (len orbitMap) 0 orbits -- Count orbit paths
  let Just numTransfers = transfers (path orbitMap [] "YOU") (path orbitMap [] "SAN") -- Count transfers
  print numTransfers
  where
    transfers :: [Text] -> [Text] -> Maybe Int
    transfers p1 p2 = do
      common <- viaNonEmpty head (p1 `L.intersect` p2)
      lp1 <- L.elemIndex common p1
      lp2 <- L.elemIndex common p2
      pure (lp1 + lp2)
