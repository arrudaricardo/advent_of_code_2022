import Data.List

main = do
    cals <- calSums . lines <$> readFile "day_01/input.txt"
    putStrLn $ "part 1: " ++ show (maximum cals)
    putStrLn $ "part 2: " ++ show (sum $ take 3 $ reverse $ sort cals)
  where
    calSums [] = []
    calSums xs = let (ax, bx) = break null xs
                  in sum (read <$> ax) : calSums (dropWhile null bx)
