import Data.List
import System.IO

parseInput :: IO [Int]
parseInput = do
  end <- isEOF
  if end
    then return []
    else do
      line <- getLine
      let num = read line :: Int
      rest <- parseInput
      return (num : rest)

main = do
  nums <- parseInput
  print ("A: " ++ show (head [x * y | x <- nums, y <- nums, y + x == 2020]))
  print ("B: " ++ show (head [x * y * z | x <- nums, y <- nums, z <- nums, y + x + z == 2020]))