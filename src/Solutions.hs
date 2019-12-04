module Solutions where

import Relude
import Lens.Micro (ix, (.~), (^?!))
import qualified Data.Text as T

day1 :: IO ()
day1 = do
  input <- lines <$> readFileText "./input/day1.txt" -- Read input lines
  let weights = fromRight 0 . readEither <$> input -- Convert to Integers
  putTextLn . show $ sum (fuel <$> weights) -- Part 1
  putTextLn . show $ sum (recFuel <$> weights) -- Part 2
  where
    fuel :: Integer -> Integer
    fuel = flip (-) 2 . flip div 3

    recFuel :: Integer -> Integer
    recFuel x | fuel x < 0 = 0
    recFuel x = let fx = fuel x in fx + recFuel fx

day2 :: IO ()
day2 =  do
  input <- T.splitOn "," <$> readFileText "./input/day2.txt" -- Read input lines
  let code = fromRight 0 . readEither <$> input -- Convert to Integers
      Just result = runWith 12 2 code -- Interpret program (Part 1)
  putTextLn $ show result -- Show result
  let (noun, verb) = runUntil 0 0 19690720 code -- Brute force search for values (Part 2)
  putTextLn $ show (100 * noun + verb) -- Show result
  where
    runUntil :: Integer -> Integer -> Integer -> [Integer] -> (Integer, Integer)
    runUntil noun verb result code | runWith noun verb code == Just result = (noun, verb)
    runUntil noun verb result code | verb < 99  = runUntil noun      (verb + 1) result code
    runUntil noun verb result code | verb == 99 = runUntil (noun + 1) 0         result code
    runUntil noun verb result code | noun < 99  = runUntil (noun + 1) verb      result code
    runUntil noun verb _ _ = error $ "(" <> show noun <> "," <> show verb <> ")"

    runWith :: Integer -> Integer -> [Integer] -> Maybe Integer
    runWith noun verb code = let c = replace 1 noun (replace 2 verb code) in viaNonEmpty head (run c c)

    replace :: Integer -> Integer -> [Integer] -> [Integer]
    replace pos val l = l & ix (fromIntegral pos) .~ val

    op :: (Integer -> Integer -> Integer) -> [Integer] -> Integer -> Integer -> Integer
    op f l idx1 idx2 = let get idx = (l ^?! ix (fromIntegral idx)) in f (get idx1) (get idx2)

    run :: [Integer] -> [Integer] -> [Integer]
    run (99 : cont) code = code
    run (c : i1 : i2 : o : cont) code | c == 1 || c == 2
      = let next = replace o (op (if c == 1 then (+) else (*)) code i1 i2) code
          in run (drop (length code - length cont) next) next
    run (c : cont) code = error $ "Unknown opcode " <> show c <> " at position " <> show (length code - length cont)