module Utility where

import qualified Data.Bits as DB
import qualified Data.Char as DC
import qualified Data.Text as DT

import qualified AsciiOps as A
import qualified Base64Ops as B
import qualified HexOps as H

theciph = "0b3637272a2b2e63622c"

base64ToHex :: [Char] -> [Char]
base64ToHex = H.int12LToHexStr . B.base64LToInt12

hexToBase64 :: [Char] -> [Char]
hexToBase64 = B.int12LToB64Str . H.hexLToInt12

asciiToHex :: [Char] -> [Char]
asciiToHex = H.int8LToHexStr . (map A.asciiToInt8)

hexToAscii :: [Char] -> [Char]
hexToAscii = (map A.int8ToAscii) . H.hexLToInt8

xorAsHex :: [Char] -> [Char] -> [Char]
xorAsHex key xs = map H.fourBitToHex $ zipWith DB.xor (map DC.digitToInt (cycle key)) $ map DC.digitToInt xs

xorAsAscii :: Int -> Char -> Char
xorAsAscii key asc = H.int8ToPrintAscii $ DB.xor key (DC.ord asc)

-- given ascii key and ascii plaintext, return list of 8-bit integers from xoring the repeated key with plaintext
xorAsciiKey :: String -> String -> [Int]
xorAsciiKey key ascStr =
    zipWith DB.xor (map A.asciiToInt8 (cycle key)) $ map A.asciiToInt8 ascStr

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

-- normalized distance on Ascii ciphertext
normKeylenDist n str = (fromIntegral (keylenDist (first2LenN n str))) / (fromIntegral n)

normKeylenHexDist n str = (fromIntegral (keylenDist (first2LenN n (hexToAscii str)))) / (fromIntegral n)

xorInt8WithString :: Int -> String -> String
xorInt8WithString key = map (xorAsAscii key)

xorInt8HexStr :: String -> Int -> String
xorInt8HexStr [] key = []
xorInt8HexStr (x1:x2:xs) key =
    let outChar = H.int8ToPrintAscii $ DB.xor (H.hexToInt8 x1 x2) key
    in outChar:xorInt8HexStr xs key
xorInt8HexStr _ key = error H.lengthErrMsg

tryKeysOnHexStr :: [Int] -> String -> [String]
tryKeysOnHexStr keys hexStr = map (xorInt8HexStr hexStr) keys

rateKeysBy :: (String -> Int) -> [Int] -> String -> [((Char, Int), String)]
rateKeysBy rateFctn keys cipherText =
    let appendKeyRating k xs = ((DC.chr k, rating), xs) where rating = rateFctn xs
    in zipWith appendKeyRating keys (tryKeysOnHexStr keys cipherText)

addkeyInfo ::
    [Int] -> String -> ([Int] -> String -> [String]) -> [(Int, String)]
addkeyInfo keys cipherText decryptFctn =
    let appendKey k xs = (k, xs)
    in zipWith appendKey keys (decryptFctn keys cipherText)

readLines xs = filter (\xs -> xs /= "") $ map DT.unpack $ DT.split (=='\n') $ DT.pack xs

