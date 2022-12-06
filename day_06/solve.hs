import Data.List

main = do
  stream <- readFile "input.txt"
  putStrLn $ "part 1: " ++ show (findStartPacket 4 stream)
  putStrLn $ "part 2: " ++ show (findStartPacket 14 stream)

findStartPacket :: Int -> String -> Int
findStartPacket k = findStartPacket' k
  where
    findStartPacket' n s =
      if length (nub $ sort $ take k s) == k
        then n
        else findStartPacket' (n + 1) (drop 1 s)
