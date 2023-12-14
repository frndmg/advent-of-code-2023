import Test.Hspec

import Data.List (findIndices, groupBy, sortOn)
import Data.Map (Map)
import qualified Data.Map as M

example1 = "O....#....\n\
           \O.OO#....#\n\
           \.....##...\n\
           \OO.#O....O\n\
           \.O.....O#.\n\
           \O.#..O.#.#\n\
           \..O..#O..O\n\
           \.......O..\n\
           \#....###..\n\
           \#OO..#...."

rocks :: Char -> [[Char]] -> [(Int, Int)]
rocks c xs = [ (x, y + 1)
             | (x, line) <- zip [1..] xs
             , y <- findIndices (== c) line ]

makePuzzle :: String -> ([(Int, Int)], [(Int, Int)], Int)
makePuzzle text = (roundedRocks, cubeRocks, height)
  where
    board = lines text
    height = length board
    roundedRocks = rocks 'O' board
    cubeRocks = rocks '#' board

load :: Int -> [(Int, Int)] -> Int
load height rocks = sum $ map (\x -> height - x + 1) $ map fst rocks

byCol :: [(Int, Int)] -> Map Int [Int]
byCol xs = go xs $ M.fromList $ [(c, []) | c <- [1..cols]]
  where
    go [] m = m
    go ((x,y):xs) m = go xs $ M.update (\v -> Just $ v ++ [x]) y m
    cols = maximum $ map snd xs

tiltNorth :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
tiltNorth rr cr = [ (x, y)
                  | (y, rr, cr) <- mix
                  , x <- tilt' rr cr 1]
  where
    rrByCol = map snd $ M.toAscList $ byCol rr
    crByCol = map snd $ M.toAscList $ byCol cr
    mix = zip3 [1..] rrByCol crByCol
    tilt' [] _ _ = []
    tilt' xs [] l = map (\(i, _) -> l + i) $ zip [0..] xs
    tilt' (x:xs) (y:ys) l
      | x < y     = l:tilt' xs (y:ys) (l + 1)
      | otherwise = tilt' (x:xs) ys (y + 1)

partOne :: String -> Int
partOne text = load height $ tiltNorth roundedRocks cubeRocks
  where
    (roundedRocks, cubeRocks, height) = makePuzzle text

main1 :: IO ()
main1 = readFile "input.txt" >>= (print . partOne)

test = hspec $ do
  describe "part one" $ do
    it "should solve the example" $ do
      partOne example1 `shouldBe` 136
