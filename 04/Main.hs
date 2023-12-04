import Text.Parsec (parse, parseTest, sepBy, sepEndBy, eof)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (digit, string, newline)
import Text.Parsec.Combinator (many1)

import qualified Data.Set as S

test :: String
test = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\n\
       \Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\n\
       \Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\n\
       \Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\n\
       \Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\n\
       \Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"

number :: Parser Int
number = do
  n <- many1 digit
  return (read n)

space = many1 $ string " "

numberList :: Parser [Int]
numberList = space *> number `sepEndBy` space

game :: Parser [([Int], [Int])]
game = (string "Card" *> space *> number *> string ":" *> ((,) <$> (numberList <* string "|") <*> numberList)) `sepEndBy` newline <* eof

makeGame :: String -> [([Int], [Int])]
makeGame text = let Right g = parse game "" text in g

points :: [([Int], [Int])] -> Int
points g = sum $ map ((\x -> 2^(x - 1)) . length) $ filter (not . null) $ map (\(x, y) -> S.toList $ S.intersection (S.fromList x) (S.fromList y)) $ g

scratchCards :: [([Int], [Int])] -> [Int]
scratchCards g = map length $ map (\(x, y) -> S.toList $ S.intersection (S.fromList x) (S.fromList y)) g

instances :: [Int] -> [Int]
instances matches = go matches (repeat 1)
  where
    go [] _ = []
    go (x:xs) (y:ys) = [y] ++ zs
      where zs = go xs ((map (+y) $ take x ys) ++ (drop x ys))

main1 :: IO ()
main1 = do
  content <- readFile "input.txt"
  print $ points $ makeGame content

main2 :: IO ()
main2 = do
  content <- readFile "input.txt"
  print $ sum $ instances $ scratchCards $ makeGame content
