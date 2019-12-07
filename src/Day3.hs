--
-- | Day 3
--
module Day3 where

import Relude
import Data.List (minimum)
import qualified Data.Text as T

day3 :: IO ()
day3 = do
  input <- lines <$> readFileText "./input/day3.txt" -- Read input lines
  let segments = foldl' segment [] . T.splitOn "," <$> input -- Extract all line segments
      intersections = catMaybes . (intersection <$>) . sequence $ segments -- Find intersections of all wires
  print $ minimum (snd <$> intersections) -- Find the minimal intersection by distance
  print $ minimum (fst <$> intersections) -- Find the minimal intersection by steps
  where
    op :: Text -> ([Int], Int, Int) -> ([Int], Int, Int, Int, Int)
    op t = let l = fromRight 0 (readEither (T.drop 1 t))
            in case T.take 1 t of
                "U" -> \(s, x, y) -> (l : s, x, y, x, y + l)
                "D" -> \(s, x, y) -> (l : s, x, y, x, y - l)
                "L" -> \(s, x, y) -> (l : s, x, y, x - l, y)
                "R" -> \(s, x, y) -> (l : s, x, y, x + l, y)
                _ -> error $ "Invalid direction: " <> t

    segment :: [([Int], Int, Int, Int, Int)] -> Text -> [([Int], Int, Int, Int, Int)]
    segment [] dir = [op dir ([], 0, 0)]
    segment l@((s, _, _, x, y):_) dir = op dir (s, x, y) : l

    intersection :: [([Int], Int, Int, Int, Int)] -> Maybe (Int, Int)
    intersection l@[(f:fs, fx1, fy1, fx2, fy2), (s:ss, sx1, sy1, sx2, sy2)]
      | fx1 == fx2 && sy1 == sy2 && -- (Vertical, Horizontal)
        fx1 >= min sx1 sx2 && fx1 <= max sx1 sx2 &&
        sy1 >= min fy1 fy2 && sy1 <= max fy1 fy2 &&
        abs fx1 + abs sy1 > 0
          = Just (abs (fy1 - sy1) + sum fs + abs (sx1 - fx1) + sum ss, abs fx1 + abs sy1)
      | fy1 == fy2 && sx1 == sx2 && -- (Horizontal, Vertical)
        fy1 >= min sy1 sy2 && fy1 <= max sy1 sy2 &&
        sx1 >= min fx1 fx2 && sx1 <= max fx1 fx2 &&
        abs fy1 + abs sx1 > 0
          = Just (abs (fx1 - sx1) + sum fs + abs (sy1 - fy1) + sum ss, abs fy1 + abs sx1)
    intersection _ = Nothing -- No intersection
