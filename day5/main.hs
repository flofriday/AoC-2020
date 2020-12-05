-- Convert the column description to a y cordinate
toX :: String -> [Int] -> [Int]
toX [] range = range
toX (c : cs) range
  | c == 'R' = toX cs (drop half range)
  | c == 'L' = toX cs (take half range)
  where
    half = length range `div` 2

-- Convert the row description to a y cordinate
toY :: String -> [Int] -> [Int]
toY [] range = range
toY (c : cs) range
  | c == 'B' = toY cs (drop half range)
  | c == 'F' = toY cs (take half range)
  where
    half = length range `div` 2

-- Convert the description to a x and y cordinate tupel
toCordinate :: String -> (Int, Int)
toCordinate text = (x, y)
  where
    y = head (toY (take 7 text) [0 .. 127])
    x = head (toX (drop 7 text) [0 .. 7])

-- Convert a cordinate to a Id
toId :: (Int, Int) -> Int
toId val = x + y * 8
  where
    (x, y) = val

-- Create a list with all seats of the plane as (x,y) tupels
allSeats :: [Int] -> [Int] -> [(Int, Int)]
allSeats [] _ = []
allSeats (x : xs) ys = zip (replicate (length ys) x) ys ++ allSeats xs ys

-- Find all free seats (or maybe not existing seats) and return them as a list
-- of (x,y) tupels
freeSeats :: [(Int, Int)] -> [(Int, Int)]
freeSeats taken = filter (`notElem` taken) all
  where
    all = allSeats [0 .. 7] [0 .. 127]

-- Filter all non existing seatIds, so that only free existing ones remain
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