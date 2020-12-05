toX :: String -> [Int] -> [Int]
toX [] range = range
toX (c : cs) range
  | c == 'R' = toX cs (drop half range)
  | c == 'L' = toX cs (take half range)
  where
    half = length range `div` 2

toY :: String -> [Int] -> [Int]
toY [] range = range
toY (c : cs) range
  | c == 'B' = toY cs (drop half range)
  | c == 'F' = toY cs (take half range)
  where
    half = length range `div` 2

toCordinate :: String -> (Int, Int)
toCordinate text = (x, y)
  where
    y = head (toY (take 7 text) [0 .. 127])
    x = head (toX (drop 7 text) [0 .. 7])

toId :: (Int, Int) -> Int
toId val = x + y * 8
  where
    (x, y) = val

allSeats :: [Int] -> [Int] -> [(Int, Int)]
allSeats [] _ = []
allSeats (x : xs) ys = zip (replicate (length ys) x) ys ++ allSeats xs ys

freeSeats :: [(Int, Int)] -> [(Int, Int)]
freeSeats taken = filter (`notElem` taken) all
  where
    all = allSeats [0 .. 7] [0 .. 127]

filterFreeIds :: [Int] -> [Int] -> [Int]
filterFreeIds free taken = filter (\x -> x + 1 `elem` taken && x - 1 `elem` taken) free

main = do
  input <- getContents
  let cordinates = map toCordinate (lines input)
  let ids = map toId cordinates
  print $ "A: " ++ show (maximum ids)
  let freeCordinates = freeSeats cordinates
  let freeIds = map toId freeCordinates
  print $ "B: " ++ show (head (filterFreeIds freeIds ids))