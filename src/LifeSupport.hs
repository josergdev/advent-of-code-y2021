module LifeSupport where

import Consumption
import Data.List
import qualified Consumption as Cons

transposedIn :: [[Bool]] -> Int -> [Bool]
transposedIn r i = transpose r !! i

cycles :: [[Bool]] -> [Int]
cycles r = take (length (transpose r)) [0 ..]

oxiRule :: [[Bool]] -> Int -> Bool
oxiRule r i = mostCommon (transposedIn r i)

oxiCriteria :: [[Bool]] -> Int -> [[Bool]]
oxiCriteria r i = filter (\a -> a !! i == oxiRule r i) r

oxi :: [[Bool]] -> Integer
oxi r = bin2dec $ head $ foldl oxiCriteria r (cycles r)

co2Rule :: [[Bool]] -> Int -> Bool
co2Rule r i = leastCommon (transposedIn r i)

co2Criteria :: [[Bool]] -> Int -> [[Bool]]
co2Criteria r i = filter (\a -> a !! i == co2Rule r i) r

co2 :: [[Bool]] -> Integer
co2 r = bin2dec $ head $ foldl co2Criteria r (cycles r)

rate :: [[Bool]] -> Integer
rate r = co2 r * oxi r
