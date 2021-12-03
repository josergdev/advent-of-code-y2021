module Consumption where

import Data.Bool
import Data.List
import Data.Ord

toBoolMatrix :: [String] -> [[Bool]]
toBoolMatrix = map toBoolList

toBoolList :: String -> [Bool]
toBoolList = map toBool

toBool :: Char -> Bool
toBool '0' = False
toBool '1' = True
toBool _ = error "Input Error"

newtype ReportItem = ReportItem [Bool] deriving (Show)

newtype Report = Report [[Bool]] deriving (Show)

toReport :: [String] -> Report
toReport = Report . toBoolMatrix

report :: [String]
report = ["110110000100", "010110011100", "001001101010", "011011101010", "101100010000", "010100110111", "010110101101", "111010110111", "100111110001", "001010110000", "011011000001", "100010100000", "000100110101", "101010001000", "111110000011", "001001111110", "010010010000", "111110110000", "110101100010", "011010010011", "010101011101", "100101111100", "010111101010", "110000101111", "100011100011", "101101110101", "001010010000", "100011001111", "010001111000", "101110000100", "111010010011", "111101010010", "000001010011", "111111110001", "111111111011", "110000011011", "100100100101", "110011110011", "100101001011", "110110101101", "000101110101", "100001101110", "101001001010", "111111001110", "101111001110", "011100110000", "001011001110", "000000110010", "101100101111", "110100000110", "110001100111", "110100000100", "011010000001", "000000110000", "100101100011", "010100101101", "001101100111", "101011000110", "101111001000", "111110111100", "000111000100", "101111000010", "011100100010", "100010010110", "011100000000", "101110110100", "001010100001", "001100100111", "001110100010", "000110111000", "000110101110", "011101011000", "111000110000", "000110100011", "000111101010", "000110111100", "000011100110", "011010001100", "100111100010", "110110000010", "000010101001", "010111000101", "011001011101", "100000000010", "101110100111", "101111000110", "100000011100", "001000001001", "100110100111", "001010000001", "010111101110", "101100011000", "000010100101", "010001001111", "100000110000", "000001000010", "100111010001", "001001100110", "101111100110", "111101110000", "001011101110", "110111010011", "110010000011", "101001100100", "000000111110", "001110011100", "010001101110", "011011110111", "100111100110", "100110000001", "001000001010", "100110111010", "000101101010", "011110011100", "011111110101", "000000101011", "111100100110", "100000010111", "000010011000", "001010111011", "011011011000", "110011101110", "101001100110", "001100010111", "110011010011", "111000101000", "110001010010", "100010111101", "000110111111", "001001110100", "010010111000", "001111010100", "101001001110", "110011010111", "000110010001", "000100101101", "001110001001", "000000111010", "001000010101", "000000001011", "101001011010", "111000000000", "101000010001", "100101111000", "101100001001", "110011011011", "000100101011", "010100110101", "000000000100", "101000010100", "011001110101", "010000100000", "010010010011", "101101000100", "111111000111", "101001111110", "101001010010", "000000101010", "000000100111", "110110100100", "110100110000", "000111100111", "010100100100", "100110100000", "010111100010", "111111010010", "101010101010", "010000111011", "101010111011", "011100101011", "100110010100", "101101101011", "000001101110", "011011011111", "111110001001", "100110101100", "011001111010", "001100110000", "001111001110", "111111111110", "101000000110", "110000111001", "000110000110", "010001010001", "000100010001", "111010111010", "101011101011", "000111000000", "001010111010", "011101100110", "110001010000", "111101011110", "010101010010", "000101010111", "111011101101", "110111011100", "011110100111", "101100101101", "110111100111", "010110010011", "110001011010", "110110110000", "011000000111", "100001100001", "011100011110", "011100011001", "011001001111", "100011011110", "110010001111", "100101101000", "001110101111", "111011101111", "101001000011", "010000000100", "001001110010", "010101000111", "111010001010", "010000111100", "100011111000", "100110010101", "100000100100", "011110000110", "010111010100", "011000100001", "101101011000", "011100001101", "000010001001", "000010101011", "100010101000", "101001011001", "111101100001", "000101111110", "000111011111", "011011011010", "001011111101", "100101010110", "001101110011", "010010110101", "100010010111", "101000011010", "001011010011", "001010111101", "110001001101", "110011001100", "001001011000", "001110111000", "110111011001", "111010110000", "101100110101", "000100100101", "101101011110", "101001110111", "000101000110", "010000010100", "011011010111", "000111111001", "110101001000", "010011101001", "111010011100", "011110010000", "110001010001", "100110110111", "001101010011", "001010001110", "111001101000", "111101000010", "101110100010", "100101111111", "100010001010", "110010000101", "011110011000", "000010111001", "111101011010", "100111000011", "001110101001", "100100110100", "011100001110", "101101100110", "001101110001", "101000100000", "110000101010", "011001110000", "101101110110", "010011000100", "010100001110", "101111101101", "100001100000", "111001111101", "111001100010", "001101001001", "001101110100", "100110101111", "000000101000", "001000111011", "110011010010", "111000001000", "000101010000", "001001011001", "101001110110", "111000000101", "010011010001", "111011001000", "100100111110", "111110001000", "001101000011", "111110011111", "111111101111", "101001011011", "110010100100", "000011100000", "001101000010", "111100001100", "001101000000", "011000100111", "000100100100", "111100011100", "011110000011", "110111001101", "110001000001", "011111110100", "101010011111", "011010110110", "001010100010", "010011100110", "010111010110", "111111110010", "011001001010", "001110100101", "010110010000", "111000110100", "000010101110", "100110000101", "110001110111", "001100001100", "010110111111", "100010010011", "111100110111", "111011101001", "011000001000", "100111001100", "100100100010", "010001111100", "100101001101", "001101111011", "000101010100", "010101101010", "000100010000", "110101110101", "001111011111", "010011111101", "111010110110", "111000000111", "001001111010", "100101110110", "110100010011", "001000001110", "110001100011", "000101010001", "101100110000", "101010011010", "011000110101", "000000010110", "011001010000", "010110100101", "010000101001", "000000111100", "010101100011", "010110001001", "101010101100", "110100001101", "010111111000", "000011100001", "100000001100", "111001100111", "001111111100", "110010101000", "100111000101", "110001101000", "010111001100", "001110110111", "001000110101", "000110001011", "011001000100", "000101000001", "101001000110", "000011101011", "110111111110", "101100000101", "000010011101", "011111010101", "000100110111", "010111011000", "001000010110", "101010111101", "110000111100", "011000011100", "101010100111", "100110110110", "010000100001", "001010101100", "001001100000", "010110000101", "100010000000", "010110101010", "101000110110", "011000000011", "111000101110", "101110101100", "000100111101", "111100001000", "011111011000", "101101101010", "111001011110", "111100100100", "000100010101", "011000011010", "010000111000", "010000011110", "101100111011", "011001100100", "000110001000", "010010000111", "001011101010", "001110000001", "010001100001", "001111100010", "000010111010", "001000101111", "100111110100", "101010110111", "010010111010", "101100010011", "011100000101", "001101100110", "011111000000", "011000110010", "001110110110", "101110001001", "000101101001", "011111111111", "100010100011", "100111000001", "111010001100", "111101000101", "000101100000", "110001010110", "111100111100", "000100110000", "101000101100", "001100100101", "001101011101", "110011110101", "011101000011", "001000010001", "110111101011", "010101001110", "100110110000", "111000111011", "011100001111", "011101000101", "011011110000", "111101001110", "000001111011", "000110110101", "110010001011", "011011001001", "011101011101", "011000100000", "110000110110", "100010111111", "010010110111", "001011110110", "011001110110", "011100110110", "110010110001", "100011011001", "000101101000", "110000000010", "010011101010", "100100011000", "000000001110", "100011101111", "100000111010", "011010111010", "100110010011", "100001011001", "110111100100", "100110011011", "101000000100", "111110100001", "100011000101", "010010001111", "010011111000", "101000100001", "000011011010", "011100101010", "101110000110", "000011111110", "010101011010", "110101010100", "101010010110", "111101110100", "101100011001", "011101101111", "001101011001", "000010101010", "001110101101", "000011110010", "101011000011", "001011110011", "101101001010", "011101001100", "111011001010", "111010001000", "010111101000", "010100100101", "000000011110", "011000011110", "011101011010", "101000101010", "111001011100", "000110110100", "010110100110", "100011100110", "100111010011", "001010111000", "101001110100", "111011011111", "001010000000", "101001011111", "010110110011", "010100100010", "011011001000", "101111011001", "000101010010", "000000110110", "011110101110", "011010111011", "110011100101", "000111101000", "110101100111", "101100001100", "110110110010", "000000100011", "010011011111", "110000111110", "111110101010", "100001001011", "001111101010", "000100000001", "001111110101", "101010001011", "111111001111", "011101001011", "101111001101", "011111011010", "011101101110", "111111000001", "110111110001", "101110010110", "101101000110", "101101001100", "001011110111", "101000001000", "010101111001", "111011010111", "010010010110", "011010001000", "011000101000", "101101010101", "110101000001", "111111001000", "111011001101", "100100010111", "111101010000", "111001011111", "011100110001", "010101011110", "101010111010", "010110101111", "100010011000", "110110011001", "010010000011", "101011101110", "011001111000", "101110101011", "111101001001", "010110010001", "011101011011", "001001000100", "111101011011", "111111010110", "010010000110", "000111110001", "011001101110", "010100111100", "100001101000", "111010010000", "110010110010", "111001100011", "011111100000", "100100000011", "111101101110", "110000001101", "010111110111", "111111100001", "001111100110", "010001001100", "001110001101", "000110010101", "101000101011", "001001111100", "101011011111", "010100110010", "100010011011", "000111000101", "111011011101", "110100100000", "110110011111", "001111110011", "100000000110", "101110001011", "000001110110", "011100010000", "110111100101", "011101100100", "000101000100", "011010100100", "101001101001", "011101101100", "110011111011", "001010001011", "011110110110", "100000111011", "010000100010", "001100010101", "010001110101", "101100000010", "101000101001", "010010010101", "101101001101", "001101000111", "111110001100", "101010100100", "011100111100", "000011010110", "011101100010", "001000111010", "001001000101", "011110101011", "100111110111", "111001000011", "101100100000", "100110100010", "101001011101", "010011111111", "100111001010", "001100101110", "110111100011", "101100110100", "010101010111", "110110111110", "010010101111", "000000000001", "000100001000", "101011100011", "101100001000", "101011100111", "110100111111", "101011100100", "000100000110", "101011000010", "011101010100", "111110111101", "110001100110", "011110100011", "110100110101", "011000100010", "000001100101", "010111001001", "001000011011", "000100111011", "100010100010", "011011000000", "111111010101", "100011111011", "011101110111", "111000011011", "111010000111", "011010111001", "110010101101", "110111001100", "111000001100", "011101010110", "110010001010", "010100110110", "111011011011", "000010111100", "010001001101", "110010100010", "011010001110", "110100001100", "100011100010", "000101110011", "110111110111", "101110110001", "111101111111", "111100100101", "011110011110", "100000111001", "100001010100", "111001001110", "111000000011", "001010100111", "011000101110", "111011101100", "100011100000", "110010000010", "001100110011", "001100111001", "101000011011", "101100001101", "000100001010", "001110000000", "101011011001", "011000001110", "111010001111", "100001101111", "010110100100", "001111110010", "000111100010", "111011100111", "110111101010", "000101110110", "010111101001", "011000110001", "100010110000", "011000010111", "100111111010", "001100110010", "000111010010", "101010000111", "100101100001", "100000101010", "101011000101", "100110011010", "100111010101", "101001000100", "100011010011", "010111110110", "011001101010", "100111100011", "000001010110", "110001110011", "010101010100", "100000001000", "000100100001", "100011001110", "001001000011", "110110100101", "110011111010", "010110110001", "000011000110", "000101111000", "111011001110", "010010001100", "111011000111", "000101110001", "011110011101", "110101111100", "000010101000", "110110010111", "001101100000", "100110010010", "001011100101", "011001001110", "110011111110", "000000010011", "011011100001", "000010101111", "110111110101", "101001001000", "110010011100", "110000111111", "110000000001", "101010100010", "100101010000", "111110111001", "010000010101", "100100010000", "000000011111", "011011110011", "010001110111", "100100011111", "011110101001", "110001000111", "010100010100", "011111001111", "110010010110", "101110110111", "110101001010", "011100010101", "000100000011", "111001011001", "001000111001", "001001110111", "111000100111", "010011100001", "010110001010", "001000101110", "011011000100", "001011111111", "000000110011", "101101101001", "001100111010", "110010111111", "010111011010", "001001011100", "001101101101", "110110110101", "011110101000", "101001010110", "111010101101", "000101101111", "100010111000", "010101111110", "111111001100", "100101110100", "001111111111", "101101000001", "100000101101", "011010001010", "110000100000", "111100100011", "101010010101", "010100100111", "010000010110", "110001011100", "010110001111", "111101000001", "110011010001", "011011000110", "011111001101", "001100000011", "010001011000", "010110111100", "010010101100", "000101000010", "011010100110", "001111110110", "011100101100", "110011001001", "110011000110", "010011110011", "001011100000", "110100000010", "110001100010", "110101001110", "001000111101", "111110101111", "100010111010", "011101001001", "000111111101", "000101101011", "100001110000", "010111110101", "110001101111", "000100110011", "111100111000", "001000110000", "100111110011", "101000111011", "101100101001", "001011001001", "010101001101", "000011101110", "011001111111", "011011111100", "010011100010", "010001000100", "111101000100", "011111000010", "001100110101", "110011111100", "000000000111", "000010001111", "101000111100", "101110001111", "011101000001", "000101011111", "000001001001", "011111101010", "100110111001", "000111110010", "011100111011", "000111011000", "001001000111", "100001010001", "000100011000", "010011100011", "011111101111", "010101110101", "101000110000", "101011011000", "001101100100", "000111011100", "101111001011", "100011010110", "010101100100", "111001010111", "000000100101", "010001010111", "111010100110", "111110010010", "011101011110", "101100101010", "100111111111", "010010100001", "010011100100", "100101010011", "000110001110", "010010001110", "010110010010", "101001101000", "010011110010", "011111010111", "111101101000", "110100011101", "000001001000", "110010100101", "111001001011", "111001111001", "110110011101", "000110001001", "101111000011", "110101111010", "001001101100", "101110111101", "100000011110", "011001001011", "110111101000", "000010010010", "011000111111", "001100101100", "111100000000", "101100100101", "010000111001", "000001010001", "101000111000", "111001100000", "000011011111", "111100100010", "000010010011", "001000110111", "011101110110", "011010110101", "011000011000", "100101111010", "001100111110", "110110111101", "111011001100", "000000000110", "110101000011", "011010001001", "011111001001", "101101010000", "101101111010", "100101000111", "011100011011", "101110111111", "101111111000", "011101001101", "000110100110", "110000110000", "110111011010", "010011011001", "000011111001", "110110100001", "110001010011", "101010010010", "111101110101", "110100110001", "001010110010", "110111000000", "001010100000", "111110110111", "111100101101", "100111110000", "111000001111", "010100000010", "000111001011", "001110001100", "100100110011", "000010100010", "011111001110", "001111011100", "111110011001", "010010110011", "110011011001", "100011110010"]

mostCommon :: Ord a => [a] -> a
mostCommon = head . maximumBy (comparing length) . group . sort

leastCommon :: Ord a => [a] -> a
leastCommon = head . minimumBy (comparing length) . group . sort

bin2dec :: [Bool] -> Integer
bin2dec = foldl (\a -> (+) (2 * a) . bool 0 1) 0

newtype Gamma = Gamma [Bool] deriving (Show)

gamma :: Report -> Gamma
gamma (Report r) = Gamma (map mostCommon (transpose r))

gammaAsDecimal :: Gamma -> Integer
gammaAsDecimal (Gamma bs) = bin2dec bs

newtype Epsilon = Epsilon [Bool] deriving (Show)

epsilon :: Report -> Epsilon
epsilon (Report r) = Epsilon (map leastCommon (transpose r))

epsilonAsDecimal :: Epsilon -> Integer
epsilonAsDecimal (Epsilon bs) = bin2dec bs

data Rate = Rate Gamma Epsilon deriving (Show)

rate :: Report -> Rate
rate r = Rate (gamma r) (epsilon r)

consumption :: Rate -> Integer
consumption (Rate g e) = gammaAsDecimal g * epsilonAsDecimal e
