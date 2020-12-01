import Data.List
import System.IO

parseInput :: IO [Int]
parseInput = do
  end <- isEOF
  if end
    then return []
    else do
      line <- getLine
      if null line
        then return []
        else do
          let num = read line :: Int
          rest <- parseInput
          return (num : rest)

main = do
  nums <- parseInput
  print ("A: " ++ show (product [x | x <- nums, y <- nums, y + x == 2020]))
  print ("B: " ++ show (product (nub [x | x <- nums, y <- nums, z <- nums, y + x + z == 2020])))