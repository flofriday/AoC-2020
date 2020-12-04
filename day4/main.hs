import Data.Char
import Data.List
--import Data.List.Split
import qualified Data.Text as T

hasAllFields :: String -> Bool
hasAllFields text = all ((== True) . (`isInfixOf` text)) fields
  where
    fields = ["ecl:", "pid:", "eyr:", "hcl:", "byr:", "iyr:", "hgt:"]

isValidField :: String -> String -> Bool
isValidField "byr" val = year >= 1920 && year <= 2002
  where
    year = read val
isValidField "iyr" val = year >= 2010 && year <= 2020
  where
    year = read val
isValidField "eyr" val = year >= 2020 && year <= 2030
  where
    year = read val
isValidField "hgt" val
  | "in" `isSuffixOf` val = num >= 59 && num <= 76
  | "cm" `isSuffixOf` val = num >= 150 && num <= 193
  | otherwise = False
  where
    num = read (filter isDigit val)
isValidField "hcl" val = head val == '#' && all ((== True) . (`elem` "0123456789abcdef")) (tail val)
isValidField "ecl" val = val `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
isValidField "pid" val = length val == 9 && all isDigit val
isValidField "cid" _ = True

isValidB :: String -> Bool
isValidB text = hasAllFields text && allValidFields
  where
    parts = words text
    allValidFields = all (== True) (map (\[f, c] -> isValidField (T.unpack f) (T.unpack c)) (map (\x -> (T.splitOn (T.pack ":") (T.pack x))) parts))

main = do
  input <- getContents
  let passports = map (T.unpack . T.replace (T.pack "\n") (T.pack " ")) (T.splitOn (T.pack "\n\n") (T.pack input))
  print $ "A: " ++ show (sum (map (fromEnum . hasAllFields) passports))
  print $ "B: " ++ show (sum (map (fromEnum . isValidB) passports))