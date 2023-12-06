import Text.Parsec (parse, parseTest, sepBy, many1, manyTill, sepEndBy, eof, try)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (string, newline, digit, letter, char, oneOf)
import Control.Applicative (many, (<|>))
import Data.List (sortOn)

mapPoint :: [(Int, Int, Int)] -> Int -> Int
mapPoint [] x = x
mapPoint ((dest, src, range):maps) x
  | src <= x && x <= src + range = dest + (x - src)
  | otherwise = mapPoint maps x

mapRange :: [(Int, Int, Int)] -> (Int, Int) -> [(Int, Int)]
mapRange [] (a, b) = [(a, b)]
mapRange ((dest, src, range):rest) (a, b)
  | aLeft && bLeft     = [(a, b)]
  | aLeft && bCenter   = [(a, src - 1), (dest, dest + (b - src))]
  | aLeft && bRight    = [(a, src - 1), (dest, dest + range - 1)] ++ mapRange rest (high + 1, b)
  | aCenter && bCenter = [(dest + (a - src), dest + (b - src))]
  | aCenter && bRight  = [(dest + (a - src), dest + range - 1)] ++ mapRange rest (high + 1, b)
  | aRight && bRight   = mapRange rest (a, b)
  | otherwise = error ("What?!" ++ show (a, b))
  where
    high = src + range - 1
    aLeft = a < src
    aCenter = src <= a && a <= high
    aRight = high < a
    bLeft = b < src
    bCenter = src <= b && b <= high
    bRight = high < b

test :: String
test = "seeds: 79 14 55 13\n\
       \\n\
       \seed-to-soil map:\n\
       \50 98 2\n\
       \52 50 48\n\
       \\n\
       \soil-to-fertilizer map:\n\
       \0 15 37\n\
       \37 52 2\n\
       \39 0 15\n\
       \\n\
       \fertilizer-to-water map:\n\
       \49 53 8\n\
       \0 11 42\n\
       \42 0 7\n\
       \57 7 4\n\
       \\n\
       \water-to-light map:\n\
       \88 18 7\n\
       \18 25 70\n\
       \\n\
       \light-to-temperature map:\n\
       \45 77 23\n\
       \81 45 19\n\
       \68 64 13\n\
       \\n\
       \temperature-to-humidity map:\n\
       \0 69 1\n\
       \1 0 69\n\
       \\n\
       \humidity-to-location map:\n\
       \60 56 37\n\
       \56 93 4"

number :: Parser Int
number = read <$> many1 digit

space :: Parser String
space = string " "

numberList :: Parser [Int]
numberList = number `sepBy` space

mapSource :: Parser (Int, Int, Int)
mapSource = (,,) <$> (number <* space) <*> (number <* space) <*> number

mapSection :: Parser [(Int, Int, Int)]
mapSection = many1 (letter <|> oneOf "- :\n") *> (mapSource `sepEndBy` newline)

parser :: Parser ([Int], [[(Int, Int, Int)]])
parser = (,) <$> (string "seeds: " *> numberList <* newline <* newline) <*> ((mapSection `sepBy` newline) <* eof)

parseMaps :: String -> ([Int], [[(Int, Int, Int)]])
parseMaps text = let Right result = parse parser "" text in result

location1 :: [[(Int, Int, Int)]] -> Int -> Int
location1 maps p = (foldr (.) id $ map mapPoint $ reverse maps) p

main1 :: IO ()
main1 = do
  content <- readFile "input.txt"
  let (seeds, maps) = parseMaps content
  print $ minimum $ map (location1 maps) seeds

ranges :: [Int] -> [(Int, Int)]
ranges [] = []
ranges (a:b:rest) = [(a,b+a-1)] ++ ranges rest

location2 :: [[(Int, Int, Int)]] -> (Int, Int) -> Int
location2 maps p = minimum $ map fst $ foldr (\x acc -> concat $ map (\y -> mapRange x y) acc) [p] $ map (sortOn (\(_, x, _) -> x)) $ reverse maps

-- location2' :: [[(Int, Int, Int)]] -> [(Int, Int)] -> [(Int, Int)]
-- location2' maps = foldr (\x acc -> concat $ map (mapRange x)) maps'
--   where maps' = map (sortOn (\(_, x, _) -> x)) $ reverse maps

main2 :: IO ()
main2 = do
  content <- readFile "input.txt"
  let (seeds, maps) = parseMaps content
  print $ minimum $ map (location2 maps) $ ranges seeds
