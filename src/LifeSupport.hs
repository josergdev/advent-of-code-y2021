module LifeSupport where

import Consumption
import Data.List
import qualified Consumption as Cons

transposedIn :: [[Bool]] -> Int -> [Bool]
transposedIn r i = transpose r !! i

cycles :: [[Bool]] -> [Int]
cycles r = take (length (head r)) [0 ..]

oxiRule :: [[Bool]] -> Int -> Bool
oxiRule rf i = mostCommon (transposedIn rf i)

co2Rule :: [[Bool]] -> Int -> Bool
co2Rule r i = leastCommon (transposedIn r i)

criteria :: ([[Bool]] -> Int -> Bool) -> [[Bool]] -> Int -> [Bool] -> Bool
criteria p rf i a = a !! i == p rf i

filtered :: ([[Bool]] -> Int -> Bool) -> [[Bool]] -> Int -> [[Bool]]
filtered p rf i = filter (criteria p rf i) rf

untilCriteria :: ([[Bool]] -> Int -> Bool) -> [[Bool]] -> [[Bool]]
untilCriteria p r = foldl (filtered p) r (cycles r)

binRate :: ([[Bool]] -> Int -> Bool) -> [[Bool]] -> [Bool]
binRate p r = head $ untilCriteria p r

oxi :: [[Bool]] -> [Bool]
oxi = binRate oxiRule

co2 :: [[Bool]] -> [Bool]
co2 = binRate co2Rule

lfRate :: [[Bool]] -> Integer
lfRate r = bin2dec (co2 r) * bin2dec (oxi r)
