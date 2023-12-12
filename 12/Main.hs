import Test.Hspec
import Data.List (intercalate, tails)

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
    -- go a b c = error $ "arrangmenets " ++ (intercalate " " $ [show a, show b, show c])

arrangements' :: [Char] -> [Int] -> Int
arrangements' xs ds = dp !! 0 !! 0 !! 0
  where
    dp = [ [ [ go xn xs dn ds c
             | c <- [0..if (null ds) then 0 else (head ds)] ]
           | (dn, ds) <- zip [0..] $ tails ds ]
         | (xn, xs) <- zip [0..] $ tails xs ]
    dp' x y z = dp !! x !! y !! z
    go _ [] _ [] c
      | c == 0    = 1
      | otherwise = 0
    go _ [] _ (d:ds) c
      | c == d && null ds = 1
      | otherwise         = 0
    go xi ('?':xs) di (d:ds) c
      | c == 0    = dp' (xi + 1) di 0 + dp' (xi + 1) di 1
      | c == d    = dp' (xi + 1) (di + 1) 0
      | otherwise = dp' (xi + 1) di (c + 1)
    go xi ('?':xs) di [] 0 = dp' (xi + 1) di 0
    go xi ('.':xs) di (d:ds) c
      | c == 0    = dp' (xi + 1) di 0
      | c == d    = dp' (xi + 1) (di + 1) 0
      | otherwise = 0
    go xi ('.':xs) di [] 0 = dp' (xi + 1) di 0
    go xi ('#':xs) di (d:ds) c
       | c < d     = dp' (xi + 1) di (c + 1)
       | otherwise = 0
    go _ ('#':xs) _ [] _ = 0

partOne :: String -> Int
partOne text = sum $ map (uncurry arrangements) $ makePuzzle text

unfoldPuzzle :: [Char] -> [Int] -> ([Char], [Int])
unfoldPuzzle xs ds =
  ( intercalate "?" $ take 5 $ repeat xs
  , concat $ take 5 $ repeat ds )

partTwo :: String -> Int
partTwo text = sum $ map (uncurry arrangements') $ map (uncurry unfoldPuzzle) $ makePuzzle text

main1 :: IO ()
main1 = readFile "input.txt" >>= (print . partOne)

main2 :: IO ()
main2 = readFile "input.txt" >>= (print . partTwo)

test :: IO ()
test = hspec $ do
  describe "part one" $ do
    it "should solve the example" $ do
      partOne example2 `shouldBe` 21
  describe "part two" $ do
    it "should solve example from part 1" $ do
      partTwo example2 `shouldBe` 525152
