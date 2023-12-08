import Text.Parsec (parse, parseTest, sepEndBy)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (string, newline, oneOf, letter, digit)
import Text.Parsec.Combinator (many1)
import Control.Applicative (many, (<|>))

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust, catMaybes)

test :: String
test = "LLR\n\
       \\n\
       \AAA = (BBB, BBB)\n\
       \BBB = (AAA, ZZZ)\n\
       \ZZZ = (ZZZ, ZZZ)"

test2 :: String
test2 = "LR\n\
        \\n\
        \11A = (11B, XXX)\n\
        \11B = (XXX, 11Z)\n\
        \11Z = (11B, XXX)\n\
        \22A = (22B, XXX)\n\
        \22B = (22C, 22C)\n\
        \22C = (22Z, 22Z)\n\
        \22Z = (22B, 22B)\n\
        \XXX = (XXX, XXX)"

type NodeID = String
newtype Graph = Graph (Map NodeID (NodeID, NodeID)) deriving Show

findZZZ :: Graph -> NodeID -> String -> [NodeID]
findZZZ _ "ZZZ" _ = []
findZZZ g@(Graph m) n (x:ins) = [n] ++ findZZZ g next ins
  where
    next
      | x == 'L' = fromJust $ fst <$> (M.lookup n m)
      | x == 'R' = fromJust $ snd <$> (M.lookup n m)

findZZZs :: Graph -> [NodeID] -> String -> [[NodeID]]
findZZZs g@(Graph m) ns (x:ins)
  | all (\n -> last n == 'Z') ns = [ns]
  | otherwise = [ns] ++ findZZZs g next ins
    where
      next
        | x == 'L' = catMaybes $ map (fst <$>) $ map ((flip M.lookup) m) ns
        | x == 'R' = catMaybes $ map (snd <$>) $ map ((flip M.lookup) m) ns

spaces = many1 $ string " "

parseID :: Parser NodeID
parseID = many1 (letter <|> digit)

parseEdge :: Parser (NodeID, (NodeID, NodeID))
parseEdge = (,) <$> (parseID <* spaces <* string "=" <* spaces) <*> ((,) <$> (string "(" *> (parseID <* string "," <* spaces)) <*> (parseID <* string ")"))

parseEdges :: Parser Graph
parseEdges = (Graph . M.fromList) <$> (parseEdge `sepEndBy` newline)

parseProblem :: Parser (String, Graph)
parseProblem = (,) <$> ((many $ oneOf "LR") <* newline <* newline) <*> parseEdges

makeProblem :: String -> (String, Graph)
makeProblem text = let Right (ins, g) = parse parseProblem "" text in (concat $ repeat ins, g)

main1 :: IO ()
main1 = do
  content <- readFile "input.txt"
  let (ins, g) = makeProblem content
  print $ length $ findZZZ g "AAA" ins

main2 :: IO ()
main2 = do
  content <- readFile "input.txt"
  let (ins, g@(Graph m)) = makeProblem test2
  print $ findZZZs g (filter (\n -> last n == 'A') (M.keys m)) ins
