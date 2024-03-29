import Test.Hspec

import qualified Data.Map as M
import qualified Data.Set as S

example1 = ".|...\\....\n\
           \|.-.\\.....\n\
           \.....|-...\n\
           \........|.\n\
           \..........\n\
           \.........\\\n\
           \..../.\\\\..\n\
           \.-.-/..|..\n\
           \.|....-|.\\\n\
           \..//.|...."

data Mirror = V   -- '-'
            | H   -- '|'
            | DLR -- '\'
            | DRL -- '/'
            | D   -- '.'
            deriving Show

mirrorFromChar :: Char -> Mirror
mirrorFromChar '-'  = H
mirrorFromChar '|'  = V
mirrorFromChar '\\' = DLR
mirrorFromChar '/'  = DRL
mirrorFromChar '.'  = D
mirrorFromChar c = error $ "What!? " ++ (show c)

data Dir = N -- '^'
         | S -- 'v'
         | E -- '>'
         | W -- '<'
         deriving (Show, Eq, Ord)

type Pos = (Int, Int)

type Map = M.Map Pos Mirror

beams :: (Pos, Dir) -> Map -> [(Pos, Dir)]
beams (pos@(x, y), dir) m =
  case M.lookup pos m of
    Nothing     -> []
    Just mirror -> case (dir, mirror) of
      (N, H)   -> [w, e]
      (S, H)   -> [w, e]
      (E, H)   -> [e]
      (W, H)   -> [w]

      (N, V)   -> [n]
      (S, V)   -> [s]
      (E, V)   -> [n, s]
      (W, V)   -> [n, s]

      (N, DLR) -> [w]
      (S, DLR) -> [e]
      (E, DLR) -> [s]
      (W, DLR) -> [n]

      (N, DRL) -> [e]
      (S, DRL) -> [w]
      (E, DRL) -> [n]
      (W, DRL) -> [s]

      (N, D)   -> [n]
      (S, D)   -> [s]
      (E, D)   -> [e]
      (W, D)   -> [w]
    where
      n = ((x - 1, y), N)
      s = ((x + 1, y), S)
      e = ((x, y + 1), E)
      w = ((x, y - 1), W)

flood :: Pos -> Dir -> Map -> [Pos]
flood init dir m = go [(init, dir)] $ S.fromList []
  where
    go [] visited = S.toList $ S.map fst visited
    go (x:next) visited
      | S.member x visited || null new = go next visited
      | otherwise = go (new ++ next) (S.insert x visited)
      where new = beams x m

makePuzzle :: String -> Map
makePuzzle text = M.fromList [ ((x, y), mirrorFromChar m')
                             | (x, line) <- zip [1..] $ lines text
                             , (y, m') <- zip [1..] $ line ]

partOne = length . (flood (1, 1) E) . makePuzzle

main1 = readFile "input.txt" >>= (print . partOne)

-- A better solution than this can be implemented
-- We can go caching the result on each node when we are done visiting,
-- this will speed up this solution substantially.
partTwo text = maximum
  $ map (\(init, dir) -> length $ flood init dir puzzle) inits'
  where puzzle = makePuzzle text
        inits' = inits height width
        height = maximum $ map fst $ M.keys puzzle
        width = maximum $ map snd $ M.keys puzzle

initsSide :: Int -> Int -> Dir -> [((Int, Int), Dir)]
initsSide height width N = map (\y -> ((height, y), N)) [1..width]
initsSide _ width S      = map (\y -> ((1, y), S)) [1..width]
initsSide height _ E     = map (\x -> ((x, 1), E)) [1..height]
initsSide height width W = map (\x -> ((x, width), W)) [1..height]

inits height width = (initsSide height width N)
                     ++ (initsSide height width S)
                     ++ (initsSide height width E)
                     ++ (initsSide height width W)

main2 = readFile "input.txt" >>= (print . partTwo)

test = hspec $ do
  describe "part one" $ do
    it "should solve the example" $ do
      partOne example1 `shouldBe` 46
  describe "part two" $ do
    it "should solve the example" $ do
      partTwo example1 `shouldBe` 51
