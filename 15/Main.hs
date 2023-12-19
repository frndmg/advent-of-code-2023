import Test.Hspec

import Text.Parsec (parse, sepEndBy)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (noneOf, oneOf)
import Text.Parsec.Combinator (many1)

import Data.Char (ord)

example1 = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"

parser :: Parser [String]
parser = (many1 $ noneOf ",\n") `sepEndBy` (oneOf ",\n")

makePuzzle :: String -> [String]
makePuzzle text = let Right puzzle = parse parser "" text in puzzle

doHash :: String -> Int
doHash = foldl hash 0

hash :: Int -> Char -> Int
hash a x = ((a + (ord x)) * 17) `mod` 256

partOne :: String -> Int
partOne text = sum $ map doHash $ makePuzzle text

main1 = readFile "input.txt" >>= (print . partOne)

test = hspec $ do
  describe "part one" $ do
    it "should solve the example" $ do
      partOne example1 `shouldBe` 1320
