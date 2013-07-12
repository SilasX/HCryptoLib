module Utility where

import qualified Data.Bits as DB
import qualified Data.Char as DC
import qualified Data.Text as DT

import qualified AsciiOps as A
import qualified Base64Ops as B
import qualified HexOps as H

base64ToHex :: [Char] -> [Char]
base64ToHex = H.int12LToHexStr . B.base64LToInt12

base64ToInt8 :: [Char] -> [Int]
base64ToInt8 = int12LToInt8L . B.base64LToInt12

hexToBase64 :: [Char] -> [Char]
hexToBase64 = B.int12LToB64Str . H.hexLToInt12

-- turn two (left, right) 12-bit integers into three 8-bit ints
-- high: most significant 8 bits of left
-- low: least significant 8 bits of right
-- mid: combine least 4 significant of left with most 4 significant of right
int12ToInt8 :: (Int, Int) -> (Int, Int, Int)
int12ToInt8 (left, right) =
    let least8 x = 255 DB..&. x
        high = DB.shiftR left 4
        mid = (+) (DB.shiftL (least8 left) 8) $ DB.shiftR right 8
        low = least8 right
    in (high, mid, low)

-- for converting a string, if there is a single 12-bit int at the
-- end, treat the 4 least significant as if they were the only bits
-- in the most significant 4 bits of an 8-bit int
int12LToInt8L :: [Int] -> [Int]
int12LToInt8L [] = []
int12LToInt8L (x1:x2:xs) =
    let (high, mid, low) = int12ToInt8 (x1, x2)
    in high:mid:low:int12LToInt8L xs
int12LToInt8L (x1:xs) =
    let high = DB.shiftR x1 4
        low = DB.shiftL (15 DB..&. x1) 4
    in [high, low]

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

