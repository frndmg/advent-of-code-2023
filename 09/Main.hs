import Text.Parsec (parse, sepEndBy, sepBy1, eof)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (newline, string, digit, char)
import Text.Parsec.Combinator (many1)
import Control.Applicative ((<|>))

test :: String
test = "0 3 6 9 12 15\n\
       \1 3 6 10 15 21\n\
       \10 13 16 21 30 45"

-- | diff
-- >>> diff [1, 2, 3, 4]
-- [1,1,1]
-- >>> diff []
-- *** Exception: Prelude.tail: empty list
diff :: [Int] -> [Int]
diff xs = map (uncurry (-)) $ zip (tail xs) (init xs)

predict :: [Int] -> Int
predict xs = sum $ map last $ takeWhile (any (/= 0)) $ iterate diff xs

extrapolate :: [Int] -> Int
extrapolate xs = foldl (\acc x -> x - acc) 0 $ reverse $ map head $ takeWhile (any (/= 0)) $ iterate diff xs

number :: Parser Int
number = read <$> (many1 $ digit <|> (char '-'))

spaces = many1 $ string " "

parseNumberList :: Parser [Int]
parseNumberList = number `sepBy1` spaces

parsePuzzle :: Parser [[Int]]
parsePuzzle = parseNumberList `sepEndBy` newline <* eof

makePuzzle :: String -> [[Int]]
makePuzzle text = let Right xss = parse parsePuzzle "" text in xss

main1 :: IO ()
main1 = do
  content <- readFile "input.txt"
  print $ sum $ map predict $ makePuzzle content

main2 :: IO ()
main2 = do
  content <- readFile "input.txt"
  print $ sum $ map extrapolate $ makePuzzle content

main :: IO ()
main = undefined
