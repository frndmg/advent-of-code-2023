import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map (Map)
import Data.Set (Set)

import Data.Maybe (isJust, fromJust, catMaybes)

import Data.List (sort)

type Pos = (Int, Int)
data Pipe = HPipe | VPipe | NEPipe | NWPipe | SEPipe | SWPipe | Ground | Init deriving (Show, Eq)
type Graph = Map Pos Pipe

pipeFromChar :: Char -> Pipe
pipeFromChar 'S' = Init
pipeFromChar '.' = Ground
pipeFromChar '|' = VPipe
pipeFromChar '-' = HPipe
pipeFromChar 'L' = NEPipe
pipeFromChar 'J' = NWPipe
pipeFromChar '7' = SWPipe
pipeFromChar 'F' = SEPipe

test :: String
test = "-L|F7\n\
       \7S-7|\n\
       \L|7||\n\
       \-L-J|\n\
       \L|-JF"

test2 = "7-F7-\n\
        \.FJ|7\n\
        \SJLL7\n\
        \|F--J\n\
        \LJ.LJ"

test3 = "...........\n\
        \.S-------7.\n\
        \.|F-----7|.\n\
        \.||.....||.\n\
        \.||.....||.\n\
        \.|L-7.F-J|.\n\
        \.|..|.|..|.\n\
        \.L--J.L--J.\n\
        \..........."

test4 = "..........\n\
        \.S------7.\n\
        \.|F----7|.\n\
        \.||....||.\n\
        \.||....||.\n\
        \.|L-7F-J|.\n\
        \.|..||..|.\n\
        \.L--JL--J.\n\
        \.........."

test5 = ".F----7F7F7F7F-7....\n\
        \.|F--7||||||||FJ....\n\
        \.||.FJ||||||||L7....\n\
        \FJL7L7LJLJ||LJ.L-7..\n\
        \L--J.L7...LJS7F-7L7.\n\
        \....F-J..F7FJ|L7L7L7\n\
        \....L7.F7||L7|.L7L7|\n\
        \.....|FJLJ|FJ|F7|.LJ\n\
        \....FJL-7.||.||||...\n\
        \....L---J.LJ.LJLJ..."

makeGraph :: String -> Graph
makeGraph text = M.fromList
  [ ((row, column), pipeFromChar x)
  | (row, line) <- zip [1..] $ lines text
  , (column, x) <- zip [1..] line ]

makeLoop :: Graph -> Pos -> [Pos]
makeLoop g initPos = go (S.fromList [initPos]) [initPos] (connectedNeighbors g initPos)
  where
    go :: Set Pos -> [Pos] -> [Pos] -> [Pos]
    go visited nodes [] = nodes
    go visited nodes (x:xs)
      | S.member x visited = go visited nodes xs
      | otherwise = go (S.insert x visited) (x:nodes) (connectedNeighbors g x)

pointsInsideLoop :: Graph -> [Pos] -> [Pos]
pointsInsideLoop g loop = filter (insideLoop g' loopSet') candidates
  where loopSet = S.fromList loop
        g' = updateInit g (last loop)
        loopSet' = S.fromList $ filter (\x -> notVPipe $ M.lookup x g) $ loop
        candidates = filter (\x -> not $ S.member x loopSet) $ M.keys g

updateInit :: Graph -> Pos -> Graph
updateInit g pos = M.update go pos g
  where
    [a, b] = connectedNeighbors g pos
    go Init = Just $ case (dirRelative pos a, dirRelative pos b) of
      (N, E) -> NEPipe
      (E, N) -> NEPipe
      (N, W) -> NWPipe
      (W, N) -> NWPipe
      (S, E) -> SEPipe
      (E, S) -> SEPipe
      (S, W) -> SWPipe
      (W, S) -> SWPipe
      (N, S) -> VPipe
      (S, N) -> VPipe
      (E, W) -> HPipe
      (W, E) -> HPipe
    go _ = Nothing

data Dir = N | S | E | W

dirRelative :: Pos -> Pos -> Dir
dirRelative a@(ax, ay) b@(bx, by)
  | ax < bx && ay == by = S
  | bx < ax && ay == by= N
  | ay < by && ax == bx = E
  | by < ay && ax == bx = W
  | otherwise = error "What?!"

notVPipe p = case p of
  Just VPipe -> False
  _ -> True

countNots :: [Pipe] -> Int
countNots [] = 0
countNots (SEPipe:NWPipe:rest) = 1 + countNots rest
countNots (SWPipe:NEPipe:rest) = 1 + countNots rest
countNots (_:rest) = 1 + countNots rest

insideLoop :: Graph -> Set Pos -> Pos -> Bool
insideLoop g loop (x, y) =
  (== 1) $ (`mod` 2) $ countNots
  $ catMaybes $ map (\x -> M.lookup x g)
  $ filter (\p -> S.member p loop)
  $ [(z, y) | z <- [1..x]]

connected :: Graph -> Pos -> Pos -> Bool
connected g x y
  | not inGraph || hConn == vConn = False
  | otherwise = case (hConn, vConn, pipeX, pipeY) of
    -- Init
    (True, _, Init, HPipe) -> True
    (True, _, Init, NWPipe) -> True
    (True, _, Init, SWPipe) -> True
    (True, _, HPipe, Init) -> True
    (True, _, NEPipe, Init) -> True
    (True, _, SEPipe, Init) -> True
    (_, True, Init, VPipe) -> True
    (_, True, Init, NEPipe) -> True
    (_, True, Init, NWPipe) -> True
    (_, True, VPipe, Init) -> True
    (_, True, SEPipe, Init) -> True
    (_, True, SWPipe, Init) -> True
    -- Horizontal
    (True, _, HPipe, HPipe) -> True
    (True, _, HPipe, SWPipe) -> True
    (True, _, HPipe, NWPipe) -> True
    (True, _, NEPipe, HPipe) -> True
    (True, _, NEPipe, SWPipe) -> True
    (True, _, NEPipe, NWPipe) -> True
    (True, _, SEPipe, HPipe) -> True
    (True, _, SEPipe, SWPipe) -> True
    (True, _, SEPipe, NWPipe) -> True
    -- Vertical
    (_, True, VPipe, VPipe) -> True
    (_, True, VPipe, NEPipe) -> True
    (_, True, VPipe, NWPipe) -> True
    (_, True, SEPipe, VPipe) -> True
    (_, True, SEPipe, NEPipe) -> True
    (_, True, SEPipe, NWPipe) -> True
    (_, True, SWPipe, VPipe) -> True
    (_, True, SWPipe, NEPipe) -> True
    (_, True, SWPipe, NWPipe) -> True
    _ -> False
  where
    inGraph = (isJust $ M.lookup x g) && (isJust $ M.lookup y g)
    [sX, sY] = sort [x, y]
    pipeX = fromJust $ M.lookup sX g
    pipeY = fromJust $ M.lookup sY g
    hConn = (abs (snd x - snd y)) == 1
    vConn = (abs (fst x - fst y)) == 1

connectedNeighbors :: Graph -> Pos -> [Pos]
connectedNeighbors g p = filter (connected g p) $ neighbors p

neighbors :: Pos -> [Pos]
neighbors (x, y) = [ ( x + 1, y )
                   , ( x - 1, y )
                   , ( x, y + 1 )
                   , ( x, y - 1 ) ]

main1 :: IO ()
main1 = do
  content <- readFile "input.txt"
  let g = makeGraph content
  let initPos = (!! 0) $ map fst $ filter (\(_, x) -> x == Init) $ M.toList g
  print $ (`div` 2) $ length $ makeLoop g initPos

main2 :: IO ()
main2 = do
  content <- readFile "input.txt"
  let g = makeGraph content
  let initPos = (!! 0) $ map fst $ filter (\(_, x) -> x == Init) $ M.toList g
  let loop = makeLoop g initPos
  print $ length $ pointsInsideLoop g loop
