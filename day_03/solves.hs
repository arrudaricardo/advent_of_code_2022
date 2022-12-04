import Data.Char
import Data.List
import Data.Maybe

main = do
  rucksacks <- lines <$> readFile "input.txt"
  putStrLn $ "part 1: " ++ show (sum $ priority . common <$> rucksacks)
  putStrLn $ "part 2: " ++ show (part2 rucksacks)
  where
    part2 [] = 0
    part2 (x1 : x2 : x3 : xs) = priority (head $ foldl1 intersect [x1, x2, x3]) + part2 xs
    common s =
      let (s1, s2) = splitAt (length s `div` 2) s
       in fromMaybe (error "invalid input") $ find (`elem` s2) s1
    priority c = ord c - if isAsciiLower c then 96 else 38
