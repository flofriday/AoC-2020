import qualified Data.Set as Set
import qualified Data.Text as T

countInGroup :: [[Char]] -> Int
countInGroup answers = Set.size (Set.fromList (concat answers))

countAllInGroup :: [[Char]] -> Int
countAllInGroup answers = sum (map (\c -> fromEnum (all (c `elem`) answers)) first)
  where
    first = head answers

main = do
  input <- getContents
  let answers = map (lines . T.unpack) (T.splitOn (T.pack "\n\n") (T.pack input))
  putStrLn $ "A: " ++ show (sum (map countInGroup answers))
  putStrLn $ "B: " ++ show (sum (map countAllInGroup answers))
