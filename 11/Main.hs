import Test.Hspec
import Data.Bits ((.|.), testBit, shiftL, shiftR)
import Data.List (findIndices, groupBy, tails)

example1 = "...#......\n\
           \.......#..\n\
           \#.........\n\
           \..........\n\
           \......#...\n\
           \.#........\n\
           \.........#\n\
           \..........\n\
           \.......#..\n\
           \#...#....."

galaxies :: String -> [(Int, Int)]
galaxies text =
  [ (x, y)
  | (x, line) <- zip [0..] $ lines text
  , y <- findIndices (== '#') $ line ]

empty :: [Int] -> [Int]
empty = findEmpty . foldr (.|.) 0 . map (1 `shiftL`)

findEmpty :: Integer -> [Int]
findEmpty x =
  -- Get the index of the ones with the first bit activated
  findIndices (not . (`testBit` 0))
  -- Get all right shifts until 0
  $ takeWhile (/= 0) $ iterate (`shiftR` 1) x

expand :: [Int] -> [Int] -> [Int]
expand = expand' 1

expand' :: Int -> [Int] -> [Int] -> [Int]
expand' c xs es = go xs es xs
  where go _ [] as = as
        go [] _ as = as
        go (x:xs) (e:es) (a:as)
          | e < x     = go (x:xs) es (map (+c) (a:as))
          | otherwise = a:go xs (e:es) as

expandUniverse :: [(Int, Int)] -> [(Int, Int)]
expandUniverse = expandUniverse' 1

expandUniverse' :: Int -> [(Int, Int)] -> [(Int, Int)]
expandUniverse' c gs =
  [ (x, y)
  | (x, columns) <- zip rows columns
  , y <- columns ]
  where
    rows = expand' c (map (fst . head) byRows) emptyRows
    columns = map (\x -> expand' c x emptyColumns) $ map (map snd) byRows

    emptyColumns = empty $ map snd gs
    emptyRows = empty $ map fst gs

    byRows = groupBy (\a b -> fst a == fst b) $ gs

distance :: (Int, Int) -> (Int, Int) -> Int
distance (ax, ay) (bx, by) = abs (ax - bx) + abs (ay - by)

pairs :: [a] -> [(a, a)]
pairs xs = [(x, y) | (x:rest) <- tails xs, y <- rest]

part1 :: String -> Int
part1 = part2 1

part2 :: Int -> String -> Int
part2 c text = sum $ map (uncurry distance) $ pairs universe
  where universe = expandUniverse' c $ galaxies text

main1 :: IO ()
main1 = do
  readFile "input.txt" >>= (print . part1)

main2 :: IO ()
main2 = do
  readFile "input.txt" >>= (print . part2 999999)

test = hspec $ do
  describe "part1" $ do
    it "part one solution should solve example" $ do
      part1 example1 `shouldBe` 374
  describe "part2" $ do
    it "part two solution should solve example for 1, 9 and 99 expansion" $ do
      part2 1 example1 `shouldBe` 374
      part2 9 example1 `shouldBe` 1030
      part2 99 example1 `shouldBe` 8410
