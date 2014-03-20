{- Library for frequency analysis of possible ciphertexts -}

module Frequency where

import qualified Data.Bits as DB
import qualified Data.Char as DC
import qualified Data.List as DL
import qualified Data.Ord as DO

import qualified AsciiOps as A
import qualified Utility as U

perCharRate :: (Char -> Bool) -> Int -> [Char] -> Int
perCharRate clsr weight cphrtxt =
    DL.foldl' (\i c -> i + if clsr c then weight else 0) 0 cphrtxt

-- check wehther a given string xs starts with a given string prs
startWith :: [Char] -> [Char] -> Bool
startWith [] _ = True
startWith (pr:prs) [] = False
startWith (pr:prs) (x:xs) =
    if pr == x then startWith prs xs else False

-- given a list of "good" groups in english return a function that counts how many such groups are in the list, weighting by a given integer
--goodGroup :: [[Char]] -> Int -> [Char] -> Int
--goodGroup groups weight =
--    foldl (\i str -> (i + if or (map (flip startWith str) groups) then weight else 0)) 0
--
-- combinator for different rating functions
(=+=) :: ([Char] -> Int) -> ([Char] -> Int) -> [Char] -> Int
(=+=) rater1 rater2 =
    \xs -> rater1 xs + rater2 xs

-- first attempt at a rating function
freqCheck1 :: [Char] -> Int
freqCheck1 = perCharRate DC.isPrint 1
         =+= perCharRate DC.isSpace 1
         =+= perCharRate (flip elem "etaoin") 1

bestKeyHexMatch :: String -> ((Char, Int), String)
bestKeyHexMatch cipherText = DL.maximumBy (DO.comparing (snd . fst)) $ U.rateKeysBy freqCheck1 [0..255] cipherText

-- For just getting the best single-char hex description
bestHexDecryption :: String -> String
bestHexDecryption = snd . bestKeyHexMatch

bestDecryption :: [((Char, Int), String)] -> ((Char, Int), String)
bestDecryption = DL.maximumBy (DO.comparing (snd . fst))

{- functions for finding the key length -}
hamDistInt8 :: Int -> Int -> Int
hamDistInt8 a b = DB.popCount $ DB.xor a b

hamDistIntL :: [Int] -> [Int] -> Int
hamDistIntL xs ys = sum $ zipWith hamDistInt8 xs ys

hamDistAscStr :: String -> String -> Int
hamDistAscStr a b = hamDistIntL (toInt8 a) (toInt8 b)
    where toInt8 = map A.asciiToInt8

-- given 2-tuple of strings return hamming distance
keylenDist :: (String, String) -> Int
keylenDist = uncurry hamDistAscStr

-- get first two substrings of length N
first2LenN :: Int -> String -> (String, String)
first2LenN n str = (take n str, take n $ drop n str)

-- return all 2-tuples neighboring n-length strings
--allNLenPairs :: Int -> String -> [(String, String)]
allNLenPairs n str =
    if length str < 2 * n
        then []
        else first2LenN n str:allNLenPairs n (drop n str)

-- implementation of average
average xs = (fromIntegral (sum xs)) / fromIntegral (length xs)

-- normalized distance on Ascii ciphertext
normKeylenDist n str =
    (fromIntegral . keylenDist . first2LenN n) str / fromIntegral n

-- same as normKeylenDist but for hex input
normKeylenHexDist n str =
    (fromIntegral . keylenDist . (first2LenN n) . U.hexToAscii) str / fromIntegral n

-- given list of keylengths n for ascii cipherText, returning hamming distance return normalized Hamming distance of first two n-length segments of cipherText
keylenDists :: Fractional a => String -> [Int] -> [a]
keylenDists cipherText = map (flip normKeylenDist cipherText)

-- same, but for hex cipherexts
keylenHexDists :: Fractional a => String -> [Int] -> [a]
keylenHexDists cipherText = map (flip normKeylenHexDist cipherText)

-- given ascii ciphertext and list of keys lengths, return most probable key
probKeylen :: String -> [Int] -> Int
probKeylen cipherText keys =
    DL.minimumBy (DO.comparing (\n -> normKeylenDist n (take 500 cipherText)) ) keys
