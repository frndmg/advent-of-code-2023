import Text.Parsec (parse, SourcePos, getPosition, sourceLine, sourceColumn, parseTest, eof, sepEndBy)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (digit, newline, satisfy)
import Text.Parsec.Combinator (many1)
import Data.Char (isDigit, isSpace)
import Control.Applicative ((<|>), many)
import Control.Monad (void, guard)
import Data.Maybe (catMaybes)
import Data.List (nub)

import qualified Data.Map as M

type Loc = (Int, Int)
data Node = Number Int Int | Symbol Char deriving Show
type Graph = M.Map Loc Node

withPosition :: Parser a -> Parser (a, SourcePos, SourcePos)
withPosition p = do
  init <- getPosition
  r <- p
  end <- getPosition
  return (r, init, end)

number :: Parser Node
number = do
  n <- many1 digit
  p <- getPosition
  return $ Number (read n) (sourceColumn p - 1)

number' = withPosition number

symbol :: Parser Node
symbol = do
  init <- getPosition
  s <- satisfy (\x -> not (isDigit x) && not (isSpace x))
  return $ Symbol s

symbol' = withPosition symbol

board :: Parser [(Node, SourcePos, SourcePos)]
board = do
  r <- many (number' <|> symbol') `sepEndBy` newline <* eof
  return $ concat r

makeGraph :: String -> Graph
makeGraph text = M.fromList [((sourceLine init, sourceColumn init), n) | (n, init, end) <- ns]
  where
    Right nodes = parse board "" text
    ns = noDots nodes

noDots :: [(Node, SourcePos, SourcePos)] -> [(Node, SourcePos, SourcePos)]
noDots [] = []
noDots ((Symbol '.', _, _):rest) = noDots rest
noDots (x:r) = (x:noDots r)

borders :: Loc -> Int -> [Loc]
borders (line, initCol) endCol = do
  l <- [line - 1, line, line + 1]
  c <- [initCol - 1..endCol + 1]
  guard (l /= line || (c == initCol - 1 || c == endCol + 1))
  return (l, c)

adjacentNumbers :: Graph -> [Int]
adjacentNumbers g = adjacentNumbers' $ M.toList g
  where adjacentNumbers' [] = []
        adjacentNumbers' ((loc, Symbol _):rest) = adjacentNumbers' rest
        adjacentNumbers' ((loc, Number x end):rest)
          | adjacentSymbol $ catMaybes $ map (\x -> M.lookup x g) $ borders loc end = (x:adjacentNumbers' rest)
          | otherwise = adjacentNumbers' rest

adjacentSymbol :: [Node] -> Bool
adjacentSymbol []                = False
adjacentSymbol (Symbol '.':rest) = adjacentSymbol rest
adjacentSymbol (Symbol _:_)      = True

graphMap :: Graph -> M.Map Loc Loc
graphMap g = M.fromList $ concat $ map go g'
  where g' = M.toList g
        go :: (Loc, Node) -> [(Loc, Loc)]
        go (_, Symbol _) = []
        go (l@(r, c), Number _ e) = do
          x <- [c..e]
          return ((r, x), l)

gearRatios :: Graph -> [Int]
gearRatios g = go
  where
    go = getRatios g'
    g' = M.toList g
    getRatios [] = []
    getRatios ((loc, Symbol _):rest) = [gearRatio g loc] ++ (getRatios rest)
    getRatios (_:rest) = getRatios rest
    gm = graphMap g

    gearRatio :: Graph -> Loc -> Int
    gearRatio g l = go
      where
        numbersLoc = nub $ catMaybes $ map parent $ borders l (snd l)
        numbers = map (\(Number x _) -> x) $ filter isNumber $ catMaybes $ map (\x -> M.lookup x g) numbersLoc
        go = if (length numbers) /= 2 then 0 else foldl ((*)) 1 numbers
        parent x = M.lookup x gm

isNumber :: Node -> Bool
isNumber (Number _ _) = True
isNumber _ = False

test :: String
test = "467..114..\n\
       \...*......\n\
       \..35..633.\n\
       \......#...\n\
       \617*......\n\
       \.....+.58.\n\
       \..592.....\n\
       \......755.\n\
       \...$.*....\n\
       \.664.598.."

main1 :: IO ()
main1 = do
  content <- readFile "input.txt"
  print $ sum $ adjacentNumbers $ makeGraph content

main2 :: IO ()
main2 = do
  content <- readFile "input.txt"
  print $ sum $ gearRatios $ makeGraph content
