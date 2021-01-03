import Data.List

count :: Eq a => a -> [a] -> Int
count x = length . filter (x ==)

differences :: [Int] -> [Int]
differences [n] = []
differences (n1 : n2 : ns) = (n2 - n1) : differences (n2 : ns)

-- Solve with dynamic programming.
-- Returrns a list where the number at index i tells you how many ways tehre
-- are to reach this index from the last index. (so a bottom-up approach)
solutionB :: [Int] -> [Int]
solutionB [] = [1]
solutionB ns
  | take 3 ns == [1, 1, 1] = sum (take 3 next) : next
  | take 2 ns == [1, 1] = sum (take 2 next) : next
  | otherwise = head next : next
  where
    next = solutionB (tail ns)

main = do
  contents <- getContents
  let nums = map read (lines contents)
  let input = [0] ++ sort nums ++ [maximum nums + 3]
  let diffs = differences input
  putStrLn $ "A: " ++ show (count 1 diffs * count 3 diffs)
  putStrLn $ "B: " ++ show (head (solutionB diffs))