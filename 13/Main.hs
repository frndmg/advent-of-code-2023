import Test.Hspec
import Data.Maybe (catMaybes)
import Data.List (findIndices, sort, tails)

import Text.Parsec (parse, sepEndBy, sepBy)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (newline, oneOf)
import Text.Parsec.Combinator (many1)

example1 = "#.##..##.\n\
           \..#.##.#.\n\
           \##......#\n\
           \##......#\n\
           \..#.##.#.\n\
           \..##..##.\n\
           \#.#.##.#.\n\
           \\n\
           \#...##..#\n\
           \#....#..#\n\
           \..##..###\n\
           \#####.##.\n\
           \#####.##.\n\
           \..##..###\n\
           \#....#..#"

lineToCoor :: [[Char]] -> [(Int, Int)]
lineToCoor xs = [ (x, y + 1)
                | (x, line) <- zip [1..] xs
                , y <- findIndices (== '#') line ]

parsePuzzle :: Parser [[(Int, Int)]]
parsePuzzle = (lineToCoor <$> (many1 (oneOf ".#")) `sepEndBy` newline) `sepBy` newline

maybeHead xs = if null xs then Nothing else Just $ head xs

findHorizontalReflection :: Int -> [(Int, Int)] -> Maybe Int
findHorizontalReflection c xs = maybeHead $ filter go [1..height - 1]
  where
    height = maximum $ map fst $ xs
    go n
      | otherwise = diff flipped secondBlock == c
      where
        blockHeight = min (height - n) n
        firstBlock = filter (\(x, _) -> (n - x) < blockHeight && x <= n) xs
        flipped = sort $ map (flipHorizontal n) firstBlock
        secondBlock = sort $ filter (\(x, _) -> (x - n) <= blockHeight && n < x) xs

-- diff [] [] = 0
-- diff xs [] = length xs
-- diff [] ys = length ys
-- diff (x:xs) (y:ys)
--   | x /= y = 1 + ((diff xs ys) `min` (diff (x:xs) ys) `min` (diff xs (y:ys)))
--   | otherwise = diff xs ys

diff xs ys = dp' 0 0
  where
    dp = [ [ go n xn m ym
           | (m, ym) <- zip [0..] (tails ys) ]
         | (n, xn) <- zip [0..] (tails xs) ]
    dp' i j = dp !! i !! j
    go _ [] _ [] = 0
    go _ xs _ [] = length xs
    go _ [] _ ys = length ys
    go i (x:xs) j (y:ys)
      | x /= y = 1 + ((dp' i (j + 1)) `min` (dp' (i + 1) j))
      | otherwise = dp' (i + 1) (j + 1)

flipHorizontal i (x, y) = (i + (i - x) + 1, y)

findVerticalReflection :: Int -> [(Int, Int)] -> Maybe Int
findVerticalReflection c = (findHorizontalReflection c . transpose)

transpose :: [(Int, Int)] -> [(Int, Int)]
transpose = map (\(x, y) -> (y, x))

makePuzzle :: String -> [[(Int, Int)]]
makePuzzle text = let Right puzzle = parse parsePuzzle "" text in puzzle

partOne :: String -> Int
partOne text = vr + hr
  where
    puzzle = makePuzzle text
    vr = sum $ catMaybes $ map (findVerticalReflection 0) puzzle
    hr = 100 * (sum $ catMaybes $ map (findHorizontalReflection 0) puzzle)

partTwo :: String -> Int
partTwo text = vr + hr
  where
    puzzle = makePuzzle text
    vr = sum $ catMaybes $ map (findVerticalReflection 1) puzzle
    hr = 100 * (sum $ catMaybes $ map (findHorizontalReflection 1) puzzle)

main1 :: IO ()
main1 = readFile "input.txt" >>= (print . partOne)

main2 :: IO ()
main2 = readFile "input.txt" >>= (print . partTwo)

test :: IO ()
test = hspec $ do
  describe "part one" $ do
    it "should solve the example" $ do
      partOne example1 `shouldBe` 405
  describe "part two" $ do
    it "should solve the example" $ do
      partTwo example1 `shouldBe` 400
