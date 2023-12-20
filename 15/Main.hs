import Test.Hspec

import Text.Parsec (parse, sepEndBy, try)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (noneOf, oneOf, digit, letter, char)
import Text.Parsec.Combinator (many1)
import Control.Applicative ((<|>))

import Data.Char (ord)
import Data.Maybe (fromMaybe)

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

example1 = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"

type Label = String
data Op = Update Label Int | Delete Label deriving Show

parser :: Parser [String]
parser = (many1 $ noneOf ",\n") `sepEndBy` (oneOf ",\n")

number :: Parser Int
number = read <$> (many1 digit)

label :: Parser Label
label = many1 letter

updateOp :: Parser Op
updateOp = Update <$> (label <* char '=') <*> number

deleteOp :: Parser Op
deleteOp = Delete <$> (label <* char '-')

parser2 :: Parser [Op]
parser2 = (try updateOp <|> deleteOp) `sepEndBy` (oneOf ",\n")

makePuzzle :: String -> [String]
makePuzzle text = let Right puzzle = parse parser "" text in puzzle

makePuzzle2 :: String -> [Op]
makePuzzle2 text = let Right puzzle = parse parser2 "" text in puzzle

doHash :: String -> Int
doHash = foldl hash 0

hash :: Int -> Char -> Int
hash a x = ((a + (ord x)) * 17) `mod` 256

opHash :: Op -> Int
opHash (Update label _) = doHash label
opHash (Delete label)   = doHash label

partOne :: String -> Int
partOne text = sum $ map doHash $ makePuzzle text

play :: [Op] -> Int
play ops = go ops (M.fromList []) (M.fromList [])
  where
    go :: [Op] -> Map Label (Int, Int, Int) -> Map Int (Set Label) -> Int
    go [] lenses _ = sum $ map focusingPower $ M.elems lenses

    go ((Update label fl):ops) lenses counts =
      let box = doHash label
          pos = fromMaybe 0 $ length <$> M.lookup box counts
      in if M.member label lenses
         then go ops
              (M.update (\(b, p, f) -> Just (b, p, fl)) label lenses)
              counts
         else go ops
              (M.insert label (box, pos, fl) lenses)
              (case M.lookup box counts of
                 Just labels -> M.update (\labels -> Just (S.insert label labels)) box counts
                 Nothing -> M.insert box (S.fromList [label]) counts)

    go ((Delete label):ops) lenses counts =
      case M.lookup label lenses of
        Just (box, pos, _) -> go ops
                              (foldr (\l acc -> M.update (update pos) l acc) lenses (labels box))
                              (M.update (\ls -> Just (S.delete label ls)) box counts)
        Nothing -> go ops lenses counts
      where
        labels box = fromMaybe [] $ S.toList <$> M.lookup box counts
        update pos (box, otherPos, fl)
          | otherPos == pos = Nothing
          | otherPos > pos = Just (box, otherPos - 1, fl)
          | otherwise = Just (box, otherPos, fl)

focusingPower (box, pos, focalLength) = (1 + box) * (1 + pos) * focalLength

partTwo text = play $ makePuzzle2 text

main1 = readFile "input.txt" >>= (print . partOne)

main2 = readFile "input.txt" >>= (print . partTwo)

test = hspec $ do
  describe "part one" $ do
    it "should solve the example" $ do
      partOne example1 `shouldBe` 1320
  describe "part two" $ do
    it "should solve the example" $ do
      partTwo example1 `shouldBe` 145
