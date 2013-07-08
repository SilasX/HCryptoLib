module Utility where

import qualified Data.Bits as DB
import qualified Data.Char as DC
import qualified HexOps as H
import qualified Table64 as Tab64

base64ToHex :: [Char] -> [Char]
base64ToHex = H.int12LToHexStr . Tab64.base64LToInt12

hexToBase64 :: [Char] -> [Char]
hexToBase64 = Tab64.int12LToB64Str . H.hexLToInt12

xorAsHex :: [Char] -> [Char] -> [Char]
xorAsHex key xs = map H.fourBitToHex $ zipWith DB.xor (map DC.digitToInt (cycle key)) $ map DC.digitToInt xs

xorAsAscii :: Int -> Char -> Char
xorAsAscii key asc = H.int8ToPrintAscii $ DB.xor key (DC.ord asc)

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

addkeyInfo ::
    [Int] -> String -> ([Int] -> String -> [String]) -> [(Int, String)]
addkeyInfo keys cipherText decryptFctn =
    let appendKey k xs = (k, xs)
    in zipWith appendKey keys (decryptFctn keys cipherText)
