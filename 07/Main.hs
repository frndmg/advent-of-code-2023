import Text.Parsec (parse, sepEndBy, count)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (newline, string, digit, oneOf)
import Text.Parsec.Combinator (many1)
import Data.List (sort, sortOn, nub)

test :: String
test = "32T3K 765\n\
       \T55J5 684\n\
       \KK677 28\n\
       \KTJJT 220\n\
       \QQQJA 483"

data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | T | J | Q | K | A deriving (Eq, Ord)

instance Show Card where
  show A = "A"
  show K = "K"
  show Q = "Q"
  show J = "J"
  show T = "T"
  show Nine = "9"
  show Eight = "8"
  show Seven = "7"
  show Six = "6"
  show Five = "5"
  show Four = "4"
  show Three = "3"
  show Two = "2"

fromChar :: Char -> Card
fromChar 'A' = A
fromChar 'K' = K
fromChar 'Q' = Q
fromChar 'J' = J
fromChar 'T' = T
fromChar '9' = Nine
fromChar '8' = Eight
fromChar '7' = Seven
fromChar '6' = Six
fromChar '5' = Five
fromChar '4' = Four
fromChar '3' = Three
fromChar '2' = Two
fromChar _ = error "Invalid card"

data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind deriving (Eq, Ord, Show)

newtype Hand = Hand [Card] deriving (Eq, Show)

getType :: Hand -> HandType
getType (Hand cards)
  | labels == 1 && counts !! 0 == 5                     = FiveOfAKind
  | labels == 2 && counts !! 0 == 4                     = FourOfAKind
  | labels == 2 && counts !! 0 == 3                     = FullHouse
  | labels == 3 && counts !! 0 == 3                     = ThreeOfAKind
  | labels == 3 && counts !! 0 == 2 && counts !! 1 == 2 = TwoPair
  | labels == 4 && counts !! 0 == 2                     = OnePair
  | labels == 5                                         = HighCard
  | otherwise = error "What?!"
  where counts = map snd $ nub $ reverse $ sortOn snd $ map (\card -> (card, length $ filter (== card) cards)) cards
        labels = length counts

instance Ord Hand where
  (<=) a@(Hand va) b@(Hand vb)
    | getType a /= getType b = getType a <= getType b
    | otherwise              = go va vb
      where
        go [] [] = True
        go (a:as) (b:bs)
          | a == b    = go as bs
          | a < b     = True
          | otherwise = False

number :: Parser Int
number = read <$> (many1 digit)

parseHand :: Parser Hand
parseHand = (Hand . (map fromChar)) <$> count 5 (oneOf "AKQJT98765432")

space = many1 $ string " "

parsePuzzle :: Parser [(Hand, Int)]
parsePuzzle = ((,) <$> (parseHand <* space) <*> number) `sepEndBy` newline

makePuzzle :: String -> [(Hand, Int)]
makePuzzle text = let Right puzzle = parse parsePuzzle "" text in puzzle

main :: IO ()
main = do
  content <- readFile "input.txt"
  print $ foldr (+) 0 $ map (uncurry (*)) $ zip [1..] $ map snd $ sortOn fst $ makePuzzle content
