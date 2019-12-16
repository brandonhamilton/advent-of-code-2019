--
-- | Day 14
--
module Day14 (day14) where

import Relude
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as H

ore :: HashMap Text (Integer, [(Text, Integer)]) -> HashMap Text Integer -> Text -> Integer -> (Integer, HashMap Text Integer)
ore rm left "ORE" wanted = (wanted, left) -- ORE is free to produce
ore rm left elem wanted | have <- H.lookupDefault 0 elem left, needed <- wanted - have =
  let multiple n c | n <= c = 1 | otherwise = ceiling (fromIntegral n / fromIntegral c)
      combine mult (e, c) (s, l) = let (s', l') = ore rm l e (c * mult) in (s + s', l')
  in if needed <= 0
       then (0, H.insert elem (have - wanted) left) -- Have enough left over, no need to mine more
        else case H.lookup elem rm of
              Nothing -> (0, left) -- Element equation not found
              Just (count, inputs) -> let m = multiple needed count in foldr (combine m) (0, H.insert elem (count*m - needed) left) inputs -- Recipe to produce enough element required

day14 :: IO ()
day14 = do
  input <- lines <$> readFileText "./input/day14.txt" -- Read input data
  let parse [count, chemical] = (,) chemical (fromRight 0 (readEither count))
      recipes = (fmap . fmap) (fmap (parse. T.splitOn " ") . T.splitOn ", ") (T.splitOn " => " <$> input) -- Parse production recipes
      equations = foldr (\(elems:[[(c, num)]]) h -> H.insert c (num, elems) h) H.empty recipes -- Generate equations
      orePerFuel = fst $ ore equations H.empty "FUEL" 1 -- Determine amount of ORE required for 1 FUEL
      estimatedBound = trillion `div` orePerFuel
      Just fuel = fuelFor (compare trillion . fst . ore equations H.empty "FUEL") (estimatedBound, estimatedBound * estimatedBound) -- Amount of FUEL for 1 trillion ORE
  print orePerFuel
  print fuel
  where
    trillion = 1000000000000
    fuelFor :: Integral a => (a -> Ordering) -> (a, a) -> Maybe a -- Binary search
    fuelFor p (low, high)
      | high < low = Just high
      | otherwise = let mid = (low + high) `div` 2
                    in case p mid of
                         LT -> fuelFor p (low, mid - 1)
                         GT -> fuelFor p (mid + 1, high)
                         EQ -> Just mid
