--
-- | Day 6
--
module Day6 where

import Relude
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.List as L

day6 :: IO ()
day6 = do
  input <- lines <$> readFileText "./input/day6.txt" -- Read input lines
  let orbits = foldl' orbit Map.empty (T.splitOn ")" <$> input) -- Extract all orbits
  print (length (foldl' (path orbits) [] (Map.keys orbits))) -- Count orbit paths
  let Just numTransfers = transfers (path orbits [] "YOU") (path orbits [] "SAN") -- Count transfers
  print numTransfers
  where
    orbit :: Map Text [Text] -> [Text] -> Map Text [Text]
    orbit m [i, o] = Map.insertWith (++) o [i] m

    path :: Map Text [Text] -> [Text] -> Text -> [Text]
    path m p k | Just i <- Map.lookup k m = p <> i <> concat (path m [] <$> i)
               | otherwise = p

    transfers :: [Text] -> [Text] -> Maybe Int
    transfers p1 p2 = do
      common <- viaNonEmpty head (p1 `L.intersect` p2)
      lp1 <- L.elemIndex common p1
      lp2 <- L.elemIndex common p2
      pure (lp1 + lp2)
