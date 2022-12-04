import Data.Char
import Text.ParserCombinators.ReadP

main = do
  pairs <- fmap parsePair . lines <$> readFile "input.txt"
  putStrLn $ "part 1: " ++ show (length $ filter (uncurry encloses) pairs)
  putStrLn $ "part 2: " ++ show (length $ filter (uncurry overlaps) pairs)
  where
    parsePair = fst . last . readP_to_S pairp

data Range = Range Int Int

encloses :: Range -> Range -> Bool
encloses (Range a b) (Range a' b') =
  (a' <= a && b <= b') || (a <= a' && b' <= b)

overlaps :: Range -> Range -> Bool
overlaps (Range a b) (Range a' b') =
  (a' <= a && a <= b') || (a' <= b && b <= b') || (a <= a' && b' <= b)

digitp :: ReadP Int
digitp = read <$> many1 (satisfy isDigit)

rangep :: ReadP Range
rangep = Range <$> (digitp <* char '-') <*> digitp

pairp :: ReadP (Range, Range)
pairp = do
  r <- rangep
  char ','
  r' <- rangep
  pure (r, r')
