module HexOps where

import qualified Data.Bits as DB
import qualified Data.Char as DC

lengthErrMsg = "not appropriate length hex string"

hexToInt8 :: Char -> Char -> Int
hexToInt8 c1 c2 = (DC.digitToInt c1)*16 + DC.digitToInt c2

hexLToInt8 :: [Char] -> [Int]
hexLToInt8 [] = []
hexLToInt8 (x1:x2:xs) =
    hexToInt8 x1 x2:hexLToInt8 xs
hexLToInt8 _ = error lengthErrMsg

int8ToHex :: Int -> (Char, Char)
int8ToHex i = (high, low) where
    high = fourBitToHex $ DB.shiftR i 4
    low = fourBitToHex $ (DB..&.) i 15

int8LToHexStr :: [Int] -> [Char]
int8LToHexStr [] = []
int8LToHexStr (x:xs) =
    let (high, low) = int8ToHex x
    in high:low:(int8LToHexStr xs)

int8ToPrintAscii :: Int -> Char
int8ToPrintAscii x =
    let outChar = DC.chr x in
        if DC.isPrint outChar
        then outChar
        else '?'

hexToInt12 :: Char -> Char -> Char -> Int
hexToInt12 c1 c2 c3 =
    256 * DC.digitToInt c1 + 16 * DC.digitToInt c2 + DC.digitToInt c3

hexLToInt12 :: [Char] -> [Int]
hexLToInt12 [] = []
hexLToInt12 (x1:x2:x3:xs) =
    hexToInt12 x1 x2 x3:hexLToInt12 xs
hexLToInt12 _ = error lengthErrMsg

fourBitToHex :: Int -> Char
fourBitToHex c | 0 <= c && c <= 9 = head $ show c
               | 10 <= c && c <= 15 = DC.chr $ 97 + (c - 10)
               | otherwise = error "invalid hex char"

int12ToHex :: Int -> (Char, Char, Char)
int12ToHex i = (high, mid, low) where
    high = fourBitToHex $ (DB..&.) 15 $ DB.shiftR i 8
    mid = fourBitToHex $ (DB..&.) 15 $ DB.shiftR i 4
    low = fourBitToHex $ (DB..&.) 15 i

int12LToHexStr :: [Int] -> [Char]
int12LToHexStr [] = []
int12LToHexStr (x:xs) =
    let (high, mid, low) = int12ToHex x
    in high:mid:low:int12LToHexStr xs
