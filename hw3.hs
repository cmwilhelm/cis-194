module Golf where

import Data.Maybe

skips :: [a] -> [[a]]
skips list = map (skipFilter list) [1..(length list)]
  where skipFilter xs f     = map fst (filter (divisibleBy f) (zip xs [1..]))
        divisibleBy f (x,n) = n `mod` f == 0

localMaxima :: [Integer] -> [Integer]
localMaxima xs = fromMaybe Nothing xs
  where fromMaybe _ []           = []
        fromMaybe _ (_:[])       = []
        fromMaybe Nothing (x:xs) = fromMaybe (Just x) xs
        fromMaybe (Just a) (x1:x2:xs)
          | x1 > a && x1 > x2 = x1 : fromMaybe (Just x1) (x2:xs)
          | otherwise         = fromMaybe (Just x1) (x2:xs)

histogram :: [Integer] -> String
histogram xs = stars (map (countTimes xs) [0..9]) ++ footer
  where countTimes xs n = length $ filter (== n) xs
        footer          = "==========\n0123456789"
        stars xs
          | maximum xs < 1 = ""
          | otherwise      = stars (map (subtract 1) xs) ++
                             map (\x -> if x > 0 then '*' else ' ') xs ++ "\n"
