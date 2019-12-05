module AdventOfCode where

import Relude
import Data.List (minimum)
import Lens.Micro (ix, (.~), (^?!))
import qualified Data.Text as T

--
-- | Day 1
--
day1 :: IO ()
day1 = do
  input <- lines <$> readFileText "./input/day1.txt" -- Read input lines
  let weights = fromRight 0 . readEither <$> input -- Convert to Integers
  print $ sum (fuel <$> weights) -- Part 1
  print $ sum (recFuel <$> weights) -- Part 2
  where
    fuel :: Int -> Int
    fuel = flip (-) 2 . flip div 3

    recFuel :: Int -> Int
    recFuel x | fuel x < 0 = 0
    recFuel x = let fx = fuel x in fx + recFuel fx

--
-- | Day 2
--
day2 :: IO ()
day2 = do
  input <- T.splitOn "," <$> readFileText "./input/day2.txt" -- Read input lines
  let code = fromRight 0 . readEither <$> input -- Convert to Integers
      Just result = runWith 12 2 code -- Interpret program (Part 1)
  print result -- Show result
  let (noun, verb) = runUntil 0 0 19690720 code -- Brute force search for values (Part 2)
  print (100 * noun + verb) -- Show result
  where
    runUntil :: Int -> Int -> Int -> [Int] -> (Int, Int)
    runUntil noun verb result code | runWith noun verb code == Just result = (noun, verb)
    runUntil noun verb result code | verb < 99  = runUntil noun      (verb + 1) result code
    runUntil noun verb result code | verb == 99 = runUntil (noun + 1) 0         result code
    runUntil noun verb result code | noun < 99  = runUntil (noun + 1) verb      result code
    runUntil noun verb _ _ = error $ "(" <> show noun <> "," <> show verb <> ")"

    runWith :: Int -> Int -> [Int] -> Maybe Int
    runWith noun verb code = let c = replace 1 noun (replace 2 verb code) in viaNonEmpty head (run c c)

    replace :: Int -> Int -> [Int] -> [Int]
    replace pos val l = l & ix (fromIntegral pos) .~ val

    op :: (Int -> Int -> Int) -> [Int] -> Int -> Int -> Int
    op f l idx1 idx2 = let get idx = (l ^?! ix (fromIntegral idx)) in f (get idx1) (get idx2)

    run :: [Int] -> [Int] -> [Int]
    run (99 : cont) code = code
    run (c : i1 : i2 : o : cont) code | c == 1 || c == 2
      = let next = replace o (op (if c == 1 then (+) else (*)) code i1 i2) code
          in run (drop (length code - length cont) next) next
    run (c : cont) code = error $ "Unknown opcode " <> show c <> " at position " <> show (length code - length cont)

--
-- | Day 3
--
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

--
-- | Day 4
--
day4 :: IO ()
day4 = do
  let range = [124075..580769] -- Generate range
      valid = filter (liftA2 (&&) adjacent increasing) (reverse . digits <$> range) -- Filter out invalid numbers in range
  print $ length valid -- Part 1
  print $ length (filter adjacentPair valid) -- Part 2
  where
    digits :: Int -> [Int]
    digits n | n < 10 = [n]
    digits n | (q, r) <- quotRem n 10 = r : digits q

    increasing :: [Int] -> Bool
    increasing = fst . foldl' (\(inc, last) curr -> (inc && curr >= last, curr)) (True, 0)

    adjacent :: [Int] -> Bool
    adjacent = liftA2 (/=) length (length . group)

    adjacentPair :: [Int] -> Bool
    adjacentPair d = let g = group d in length g /= length d && any ((== 2) . length) g
