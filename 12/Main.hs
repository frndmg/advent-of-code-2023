import Test.Hspec
import Data.List (intercalate)

import Text.Parsec (parse, sepEndBy, sepBy)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (digit, string, oneOf, newline, char)
import Text.Parsec.Combinator (many1)

example1 :: String
example1 = "#.#.### 1,1,3\n\
          \.#...#....###. 1,1,3\n\
          \.#.###.#.###### 1,3,1,6\n\
          \####.#...#... 4,1,1\n\
          \#....######..#####. 1,6,5\n\
          \.###.##....# 3,2,1"

example2 :: String
example2 = "???.### 1,1,3\n\
           \.??..??...?##. 1,1,3\n\
           \?#?#?#?#?#?#?#? 1,3,1,6\n\
           \????.#...#... 4,1,1\n\
           \????.######..#####. 1,6,5\n\
           \?###???????? 3,2,1"

space = string " "

numberList :: Parser [Int]
numberList = (read <$> (many1 digit)) `sepBy` char ','

parsePuzzle :: Parser [([Char], [Int])]
parsePuzzle = ((,) <$> (many1 (oneOf "#.?") <* space) <*> numberList) `sepEndBy` newline

makePuzzle :: String -> [([Char], [Int])]
makePuzzle text = let Right p = parse parsePuzzle "" text in p

countDamaged :: [Char] -> [Int]
countDamaged xs = go xs 0
  where go [] 0 = []
        go [] c = [c]
        go ('#':xs) c = go xs (c + 1)
        go ('.':xs) 0 = go xs 0
        go ('.':xs) c = [c] ++ go xs 0

arrangements :: [Char] -> [Int] -> Int
arrangements xs ds = go xs ds 0
  where
    go [] [] c
      | c == 0    = 1
      | otherwise = 0
    go [] (d:ds) c
      | c == d && null ds = 1
      | otherwise         = 0
    go ('?':xs) (d:ds) c
      | c == 0    = go xs (d:ds) 0 + go xs (d:ds) 1
      | c == d    = go xs ds 0
      | otherwise = go xs (d:ds) (c + 1)
    go ('?':xs) [] 0 = go xs [] 0
    go ('.':xs) (d:ds) c
      | c == 0    = go xs (d:ds) 0
      | c == d    = go xs ds 0
      | otherwise = 0
    go ('.':xs) [] 0 = go xs [] 0
    go ('#':xs) (d:ds) c
      | c < d     = go xs (d:ds) (c + 1)
      | otherwise = 0
    go ('#':xs) [] _ = 0
    go a b c = error $ "arrangmenets " ++ (intercalate " " $ [show a, show b, show c])

partOne :: String -> Int
partOne text = sum $ map (uncurry arrangements) $ makePuzzle text

main1 :: IO ()
main1 = readFile "input.txt" >>= (print . partOne)

test :: IO ()
test = hspec $ do
  describe "part one" $ do
    it "should solve the example" $ do
      partOne example2 `shouldBe` 21
