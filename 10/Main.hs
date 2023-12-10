import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map (Map)
import Data.Set (Set)

import Data.Maybe (isJust, fromJust)

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
