module HexOps where

import qualified Data.Char as DC

hexToAscInt :: Char -> Char -> Int
hexToAscInt c1 c2 = (DC.digitToInt c1)*16 + DC.digitToInt c2

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
