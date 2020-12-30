import Control.Applicative
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import Text.ParserCombinators.ReadP as ReadP

data OpCode = OpCode
  { name :: String,
    value :: Int
  }
  deriving (Show)

parseLine :: ReadP OpCode
parseLine = do
  name <- many1 (satisfy isAsciiLower)
  char ' '
  ReadP.optional (char '+')
  val <- fmap read (many1 (satisfy isDigit <|> char '-'))
  eof
  return (OpCode name val)

data State = State
  { codes :: [OpCode],
    opInd :: Int,
    acc :: Int,
    vidisted :: Set.Set Int,
    canModify :: Bool
  }
  deriving (Show)

simulateA :: State -> Int
simulateA (State codes optind acc visited _)
  | Set.member optind visited = acc
  | codeName == "nop" = simulateA (State codes (optind + 1) acc nextVisited False)
  | codeName == "acc" = simulateA (State codes (optind + 1) (acc + codeValue) nextVisited False)
  | codeName == "jmp" = simulateA (State codes (optind + codeValue) acc nextVisited False)
  where
    nextVisited = Set.insert optind visited
    code = codes !! optind
    codeName = name code
    codeValue = value code

simulateB :: State -> Maybe Int
simulateB (State codes optind acc visited canModify)
  | optind == length codes = Just acc
  | Set.member optind visited = Nothing
  | codeName == "nop" && isNothing nextNop && canModify = simulateB (State codes (optind + codeValue) acc nextVisited False)
  | codeName == "nop" = nextNop
  | codeName == "jmp" && isNothing nextJmp && canModify = simulateB (State codes (optind + 1) acc nextVisited False)
  | codeName == "jmp" = nextJmp
  | codeName == "acc" = simulateB (State codes (optind + 1) (acc + codeValue) nextVisited canModify)
  where
    nextVisited = Set.insert optind visited
    nextNop = simulateB (State codes (optind + 1) acc nextVisited canModify)
    nextJmp = simulateB (State codes (optind + codeValue) acc nextVisited canModify)
    code = codes !! optind
    codeName = name code
    codeValue = value code

main = do
  input <- getContents
  let opCodes = map (fst . head . readP_to_S parseLine) (lines input)
  putStrLn $ "A: " ++ show (simulateA (State opCodes 0 0 Set.empty False))
  putStrLn $ "B: " ++ show (fromJust (simulateB (State opCodes 0 0 Set.empty True)))
