module Homework1 where

import Data.Char


-- Exercise #1

toDigits :: Integer -> [Integer]
toDigits n | n <= 0    = []
           | otherwise = (map convertDigit . show) n
  where convertDigit   = fromIntegral . digitToInt

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits


-- Exercise #2

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther nums = zipWith doubleEvens nums [1..]
  where doubleEvens a b = if even b then a * 2 else a


-- Exercise #3

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits


-- Exercise #4

validate :: Integer -> Bool
validate = divisibleByTen
         . sumDigits
         . doubleEveryOther
         . toDigitsRev
  where divisibleByTen = (\n -> n `mod` 10 == 0)


-- Exercise #5 -- Tower of Hanoi

type Peg  = Char
type Move = (Peg, Peg)

hanoi :: Int -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = moveSmallersOut ++ moveBase ++ moveSmallersOn
  where moveSmallersOut = hanoi (n-1) a c b
        moveBase        = [(a, c)]
        moveSmallersOn  = hanoi (n-1) b a c
