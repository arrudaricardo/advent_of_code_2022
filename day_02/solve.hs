
main = do
    rows <- lines <$> readFile "input.txt"
    putStrLn $ "part 1: " ++ show (sum $ score <$> rows)
    putStrLn $ "part 2: " ++ show (sum $ score' <$> rows)
  where
    score "A X" = 4
    score "B X" = 1
    score "C X" = 7
    score "A Y" = 8
    score "B Y" = 5
    score "C Y" = 2
    score "A Z" = 3
    score "B Z" = 9
    score "C Z" = 6
    score' "A X" = 3
    score' "B X" = 1
    score' "C X" = 2
    score' "A Y" = 4
    score' "B Y" = 5
    score' "C Y" = 6
    score' "A Z" = 8
    score' "B Z" = 9
    score' "C Z" = 7
