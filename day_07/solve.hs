import Data.List
import Data.Map qualified as M
import Data.Maybe

data Tree = File FileSize | Dir (M.Map String Tree)

type Path = [String]

type FileName = String

type FileSize = Int

main = do
  tree <- interpret [] emptyDir . lines <$> readFile "input.txt"
  let ds = dirSizes tree
  putStrLn $ "part 1: " ++ show (sum $ filter (<= 100000) ds)
  let needed = 30000000 - (70000000 - fsize tree)
  putStrLn $ "part 2: " ++ show (minimum $ filter (>= needed) ds)

dirSizes :: Tree -> [FileSize]
dirSizes (File s) = []
dirSizes (Dir m) = sum (fsize <$> m) : concatMap dirSizes m

fsize :: Tree -> FileSize
fsize (File s) = s
fsize (Dir ds) = sum $ fsize <$> ds

emptyDir :: Tree
emptyDir = Dir M.empty

treeInsert :: Path -> FileName -> FileSize -> Tree -> Tree
treeInsert [] fileName fileSize (Dir m) = Dir $ M.insert fileName (File fileSize) m
treeInsert (d : ds) fileName fileSize (Dir m) =
  Dir $ M.alter insertTree' d m
  where
    insertTree' :: Maybe Tree -> Maybe Tree
    insertTree' = Just . treeInsert ds fileName fileSize . fromMaybe emptyDir

interpret :: Path -> Tree -> [String] -> Tree
interpret _ t [] = t
interpret _ t ("$ cd /" : cs) = interpret [] t cs
interpret p t ("$ cd .." : cs) = interpret (init p) t cs
interpret p t (c : cs)
  | "$ cd " `isPrefixOf` c = interpret (p ++ [drop 5 c]) t cs
interpret p t ("$ ls" : cs) = interpret p t' cs'
  where
    (listings, cs') = break ("$" `isPrefixOf`) cs
    t' = foldr insertListing t listings
    insertListing :: String -> Tree -> Tree
    insertListing listing tree =
      case words listing of
        ["dir", _] -> tree
        [fileSize, fileName] ->
          treeInsert p fileName (read fileSize) tree
