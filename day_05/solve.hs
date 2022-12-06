import Data.Char
import Data.List

type CrateStack = [Char]

data MoveCommand = MoveCommand Int Int Int

main = do
  (cratesLines, _ : movesLines) <- break null . lines <$> readFile "input.txt"
  let stacks = parseStacks $ parseStackLine <$> cratesLines
      moveCommands = parseMoveCommand <$> movesLines
      stacks1 = interpret (translateTo9000 moveCommands) stacks
      stacks2 = interpret moveCommands stacks
  putStrLn $ "part 1: " ++ map head stacks1
  putStrLn $ "part 2: " ++ map head stacks2

interpret :: [MoveCommand] -> [CrateStack] -> [CrateStack]
interpret [] s = s
interpret (MoveCommand n f t : mcs) stacks =
  let c = take n $ stacks !! (f - 1)
      updateStack :: Int -> CrateStack -> CrateStack
      updateStack i s
        | i == f = drop n s
        | i == t = c ++ s
        | otherwise = s
   in interpret mcs $ zipWith updateStack [1 ..] stacks

translateTo9000  :: [MoveCommand] -> [MoveCommand]
translateTo9000 = concatMap (\(MoveCommand n f t) -> replicate n $ MoveCommand 1 f t)

parseStackLine :: String -> [Char]
parseStackLine s = (s !!) <$> [1, 5 .. length s]

parseStacks :: [[Char]] -> [CrateStack]
parseStacks = map (dropWhile isSpace) . transpose

parseMoveCommand :: String -> MoveCommand
parseMoveCommand s =
  let [_, n, _, f, _, t] = words s
   in MoveCommand (read n) (read f) (read t)