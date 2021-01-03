import Data.Char
import Text.ParserCombinators.ReadP

data Password = Password
  { min :: Int,
    max :: Int,
    special :: Char,
    text :: String
  }
  deriving (Show)

parseLine :: ReadP Password
parseLine = do
  min <- fmap read (many1 (satisfy isDigit))
  satisfy (== '-')
  max <- fmap read (many1 (satisfy isDigit))
  satisfy (== ' ')
  special <- satisfy isAsciiLower
  satisfy (== ':')
  satisfy (== ' ')
  text <- many1 (satisfy isAsciiLower)
  eof
  return (Password min max special text)

isValidA :: Password -> Bool
isValidA (Password min max char text) = min <= num && max >= num
  where
    num = length (filter (== char) text)

isValidB :: Password -> Bool
isValidB (Password min max char text)
  | a == b = False
  | a == char || b == char = True
  | otherwise = False
  where
    a = text !! (min -1)
    b = text !! (max -1)

main = do
  input <- getContents
  let passwords = map (fst . head . readP_to_S parseLine) (lines input)
  print ("A: " ++ show (sum (map (fromEnum . isValidA) passwords)))
  print ("B: " ++ show (sum (map (fromEnum . isValidB) passwords)))