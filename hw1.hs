import Data.Char


-- Exercise #1

toDigits :: (Integral a, Show a) => a -> [Int]
toDigits x = map digitToInt (show x)

toDigitsRev :: (Integral a, Show a) => a -> [Int]
toDigitsRev = reverse . toDigits


-- Exercise #2

doubleEveryOther :: [Int] -> [Int]
doubleEveryOther nums = zipWith doubleEvens nums [1..]
  where doubleEvens a b = if even b then a * 2 else a


-- Exercise #3

sumDigits :: [Int] -> Int
sumDigits []     = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs


-- Exercise #4

validate :: (Integral a, Show a) => a -> Bool
validate cardNumber  = summedDigits `mod` 10 == 0
  where summedDigits = (sumDigits . doubleEveryOther . toDigitsRev) cardNumber


-- Exercise #5 -- Tower of Hanoi

type Peg  = Char
type Move = (Peg, Peg)

hanoi :: Int -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = moveSmallersOut ++ moveBase ++ moveSmallersOn
  where moveSmallersOut = hanoi (n-1) a c b
        moveBase        = [(a, c)]
        moveSmallersOn  = hanoi (n-1) b a c
