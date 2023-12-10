{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text, split, splitOn)
import qualified Data.Text as T
import Control.Monad
import Data.Maybe (catMaybes)
import Test.DocTest (doctest)

test :: Text
test = "1abc2\n\
       \pqr3stu8vwx\n\
       \a1b2c3d4e5f\n\
       \treb7uchet"

test2 :: Text
test2 = "two1nine\n\
        \eightwothree\n\
        \abcone2threexyz\n\
        \xtwone3four\n\
        \4nineeightseven2\n\
        \zoneight234\n\
        \7pqrstsixteen"

digit :: Char -> Bool
digit '1' = True
digit '2' = True
digit '3' = True
digit '4' = True
digit '5' = True
digit '6' = True
digit '7' = True
digit '8' = True
digit '9' = True
digit _ = False

startsWithDigit :: Text -> Maybe Char
startsWithDigit x
  | T.isPrefixOf "one"   x || T.isPrefixOf "1" x = Just '1'
  | T.isPrefixOf "two"   x || T.isPrefixOf "2" x = Just '2'
  | T.isPrefixOf "three" x || T.isPrefixOf "3" x = Just '3'
  | T.isPrefixOf "four"  x || T.isPrefixOf "4" x = Just '4'
  | T.isPrefixOf "five"  x || T.isPrefixOf "5" x = Just '5'
  | T.isPrefixOf "six"   x || T.isPrefixOf "6" x = Just '6'
  | T.isPrefixOf "seven" x || T.isPrefixOf "7" x = Just '7'
  | T.isPrefixOf "eight" x || T.isPrefixOf "8" x = Just '8'
  | T.isPrefixOf "nine"  x || T.isPrefixOf "9" x = Just '9'
startsWithDigit _ = Nothing

calibrationValue :: Text -> Int
calibrationValue x = read ([a] ++ [b])
  where y = T.filter digit x
        a = T.head y
        b = T.last y

calibrationValue2 :: Text -> Int
calibrationValue2 x = read ([a] ++ [b])
  where y = catMaybes $ map startsWithDigit $ x:T.tails x
        a = head y
        b = last y

solve1 :: Text -> Int
solve1 x = sum $ map calibrationValue $ splitOn "\n" x

solve2 :: Text -> Int
solve2 x = sum $ map calibrationValue2 $ splitOn "\n" x

-- |
-- >>> solve1 $ test
-- 142
part1 :: IO ()
part1 = do
  contents <- readFile "input.txt"
  print $ solve1 $ T.pack contents

-- |
-- >>> solve2 $ test2
-- 281
part2 :: IO ()
part2 = do
  contents <- readFile "input.txt"
  print $ solve2 $ T.pack contents

main :: IO ()
main = undefined

runTests = doctest ["./Main.hs"]
