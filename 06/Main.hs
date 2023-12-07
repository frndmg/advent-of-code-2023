import Text.Parsec (parse, parseTest, sepBy)
import Text.Parsec.Char (string, digit, oneOf, letter, newline)
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator (many1)
import Control.Applicative (many)
import Data.List (sort)

-- eq t d x = - x ^ 2 +  t * x - d > 0
-- t and d are fixed, solution is values of x such that
waysToWin :: (Int, Int) -> Int
waysToWin (t, d) = length $ filter (> d) $ map (\x -> x * (t - x)) [0..t]

eq t d x = -x^2 + t*x - d

waysToWin2 :: (Int, Int) -> Int
waysToWin2 (t, d) = go rs
  where go [] = 0
        go (r:s:[]) = (ceiling (s - 1)) - (floor (r + 1)) + 1
        rs = sort $ filter (>= 0) $ solutions (-1) (fromIntegral t) (fromIntegral (-d))

solutions :: (Floating a) => a -> a -> a -> [a]
solutions a b c = [ (-b - (sqrt (b ^ 2 - 4 * a * c))) / (2 * a)
                  , (-b + (sqrt (b ^ 2 - 4 * a * c))) / (2 * a) ]

test :: String
test = "Time:      7  15   30\n\
       \Distance:  9  40  200"

number :: Parser Int
number = read <$> many1 digit

spaces = many $ string " "

numberList :: Parser [Int]
numberList = spaces *> (number `sepBy` spaces)

parseLine :: Parser [Int]
parseLine = many letter *> string ":" *> numberList

parseRaces :: Parser ([Int], [Int])
parseRaces = (,) <$> (parseLine <* newline) <*> parseLine

makeRaces :: String -> [(Int, Int)]
makeRaces text = let Right (t, d) = parse parseRaces "" text in zip t d

numberSplited :: Parser Int
numberSplited = (read . concat) <$> (spaces *> ((many1 digit) `sepBy` spaces))

parseLine2 :: Parser Int
parseLine2 = many letter *> string ":" *> numberSplited

parseRace :: Parser (Int, Int)
parseRace = (,) <$> (parseLine2 <* newline) <*> parseLine2

makeRace text = let Right (t, d) = parse parseRace "" text in (t, d)

main1 :: IO ()
main1 = do
  content <- readFile "input.txt"
  print $ foldr (*) 1 $ map waysToWin2 $ makeRaces test

main2 :: IO ()
main2 = do
  content <- readFile "input.txt"
  print $ waysToWin2 $ makeRace content
