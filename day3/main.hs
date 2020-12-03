countTrees :: [[Char]] -> Int -> Int -> Int -> Int -> Int
countTrees forest x y stepx stepy
  | y >= height = 0
  | otherwise = fromEnum isTree + countTrees forest (x + stepx) (y + stepy) stepx stepy
  where
    width = length (head forest)
    height = length forest
    isTree = forest !! y !! (x `mod` width) == '#'

main = do
  content <- getContents
  let forest = lines content
  print $ "A: " ++ show (countTrees forest 0 0 3 1)
  print $ "B: " ++ show (product (map (uncurry (countTrees forest 0 0)) [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]))
