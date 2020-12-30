import Control.Applicative
import Data.Char
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Text.ParserCombinators.ReadP

parseContains :: ReadP (Int, String)
parseContains = do
  char ' '
  cnt <- fmap read (many1 (satisfy isDigit))
  char ' '
  target <- many1 (satisfy (/= ','))
  string " bag"
  Text.ParserCombinators.ReadP.optional (satisfy (== 's'))
  return (cnt, target)

parseContainsNothing :: ReadP [(Int, String)]
parseContainsNothing = do
  string " no other bags"
  return []

parseLine :: ReadP (String, [(Int, String)])
parseLine = do
  name <- many1 get
  string " bags contain"
  targets <- parseContains `sepBy1` char ',' <|> parseContainsNothing
  char '.'
  eof
  return (name, targets)

parseInput :: String -> Map.Map String [(Int, String)]
parseInput content = do
  let parsed = [fst (head (readP_to_S parseLine line)) | line <- lines content]
  Map.fromList parsed

findSolutionOne :: Map.Map String [(Int, String)] -> String -> Bool
findSolutionOne all key
  | null targets = False
  | any ((== "shiny gold") . snd) targets = True
  | otherwise = any ((== True) . findSolutionOne all . snd) targets
  where
    targets = fromJust $ Map.lookup key all

findSolutionTwo :: Map.Map String [(Int, String)] -> String -> Int
findSolutionTwo all key
  | null targets = 0
  | otherwise = numbers + sum (map (\(cnt, key) -> cnt * findSolutionTwo all key) targets)
  where
    targets = fromJust $ Map.lookup key all
    targetCounts = map fst targets
    numbers = sum targetCounts
    targetKeys = map snd targets

main = do
  input <- getContents
  let all = parseInput input
  putStrLn $ "A: " ++ show (length $ filter (== True) $ map (findSolutionOne all) (Map.keys all))
  putStrLn $ "B: " ++ show (findSolutionTwo all "shiny gold")