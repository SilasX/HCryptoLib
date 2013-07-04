module HexOps where

import qualified Data.Bits as DB
import qualified Data.Char as DC

hexToAscInt :: Char -> Char -> Int
hexToAscInt c1 c2 = (DC.digitToInt c1)*16 + DC.digitToInt c2

hexLToInt12 :: [Char] -> [Int]
hexLToInt12 [] = []
hexLToInt12 (x1:x2:x3:xs) =
    (x1 * 256 + x2 * 16 + x3):(hexLToInt12 xs)
hexLToInt12 _ = error "not appropriate length hex string"

fourBitToHex :: Int -> Char
fourBitToHex c | 0 <= c && c <= 9 = head $ show c
               | 10 <= c && c <= 15 = DC.chr $ 97 + (c - 10)
               | otherwise = error "invalid hex char"

intToHex :: Int -> (Char, Char)
intToHex i = (high, low) where
    high = fourBitToHex $ DB.shiftR 4 i 
    low = fourBitToHex $ (DB..&.) 15 i

int12ToHex :: Int -> (Char, Char, Char)
int12ToHex i = (high, mid, low) where
    high = fourBitToHex $ (DB..&.) 15 $ DB.shiftR 8 i
    mid = fourBitToHex $ (DB..&.) 15 $ DB.shiftR 4 i
    low = fourBitToHex $ (DB..&.) 15 i

int12LToHexStr :: [Int] -> [Char]
int12LToHexStr [] = []
int12LToHexStr (x:xs) =
    let (high, mid, low) = int12ToHex x
    in high:mid:low:(int12LToHexStr xs)

intLToHexStr :: [Int] -> [Char]
intLToHexStr [] = []
intLToHexStr (x:xs) =
    let (high, low) = intToHex x
    in high:low:(intLToHexStr xs)

readHexToAscInt :: String -> [Int]
readHexToAscInt xs = let
    normReadHexToAscInt [] = []
    normReadHexToAscInt (x1:x2:xs) = (hexToAscInt x1 x2) : normReadHexToAscInt xs
    in
        if (length xs `mod` 2) /= 0
        then error "only even hex strings allowed"
        else normReadHexToAscInt xs

intToPrintAscii :: Int -> Char
intToPrintAscii x =
    let outChar = DC.chr x in
        if DC.isPrint outChar
        then outChar
        else '?'
