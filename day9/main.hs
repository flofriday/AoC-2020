import Data.List
import qualified Data.Set as Set

solveA :: [Int] -> [Int] -> Int
solveA pre (x : xs)
  | all (== False) (map (\n -> Set.member (x - n) preSet) pre) = x
  | otherwise = solveA nextPre xs
  where
    preSet = Set.fromList pre
    nextPre = tail pre ++ [x]

solveB :: [Int] -> Int -> Int -> Int -> Int
solveB numbers target index len
  | s == target = numbers !! index + numbers !! (index + len - 1)
  | s > target = solveB numbers target (index + 1) 1
  | s < target = solveB numbers target index (len + 1)
  where
    s = sum $ take len $ drop index numbers

main = do
  input <- getContents
  let numbers = fmap read (lines input)
  let preamble = take 25 numbers
  let rest = drop 25 numbers
  let resA = solveA preamble rest
  print resA
  print $ solveB numbers resA 0 1