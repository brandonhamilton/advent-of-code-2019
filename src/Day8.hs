--
-- | Day 8
--
module Day8 where

import Relude
import qualified Data.List as L
import qualified Data.Text as T

day8 :: IO ()
day8 = do
  layers <- T.chunksOf (25 * 6) <$> readFileText "./input/day8.txt" -- Read input data
  let chars = (T.length .) . T.filter . (==) -- Count chars in layer
  let checksum = L.minimumBy (compare `on` chars '0') layers -- Find layer with fewest '0' digits
  print $ chars '1' checksum * chars '2' checksum -- Calculate checksum
  let image = L.foldr1 (T.zipWith (\t b -> if t == '2' then b else t)) layers -- Merge layers into image
  mapM_ putTextLn (T.chunksOf 25 (T.map (\c -> if c == '1' then '█' else ' ') image)) -- Render image
