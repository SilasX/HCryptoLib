module Base64Ops where

import qualified Data.Bits as DB
import qualified Data.Char as DC

offsetBy :: Int -> Char -> Int
offsetBy n c = (-) (DC.ord c) n

base64ToInt :: Char -> Int
base64ToInt c | c >= 'A' && c <= 'Z' = offsetBy 65 c
              | c >= 'a' && c <= 'z' = 26 + offsetBy 97 c
              | c >= '0' && c <= '9' = 52 + offsetBy 48 c
              | c == '+' = 62
              | c == '/' = 63
              | c == '=' = 0
              | otherwise = error "invalid base 64 char"

isBase64 :: Char -> Bool
isBase64 a = or
    [elem a ['A'..'Z'], elem a ['a'..'z'],
     elem a ['0'..'9'], elem a "+/="]

base64LToInt12 :: String -> [Int]
base64LToInt12 [] = []
base64LToInt12 (x1:x2:xs) =
    (64*(base64ToInt x1) + base64ToInt x2):(base64LToInt12 xs)
base64LToInt12 _ = error "not appropriate length base 64 string"

base64ToInt24 :: Char -> Char -> Char -> Int
base64ToInt24 c1 c2 c3 =
    let low = base64ToInt c3
        mid = DB.shiftL (base64ToInt c2) 8
        high = DB.shiftL (base64ToInt c1) 16
    in low + mid + high

-- assumes that base64 string length has been padded to a multiple of 3
base64LToInt24L :: String -> [Int]
base64LToInt24L [] = []
base64LToInt24L (c1:c2:c3:cs) = base64ToInt24 c1 c2 c3:base64LToInt24L cs
base64LToInt24L _ = error "base 64 string length not mutliple of 3"

least6 :: Int -> Int
least6 = (DB..&.) 63

int24ToBase64 :: Int -> (Char, Char, Char, Char)
int24ToBase64 n =
    let charLeast6 = (intToBase64 . least6)
        c4 = charLeast6 n
        c3 = charLeast6 $ DB.shiftR n 6
        c2 = charLeast6 $ DB.shiftR n 12
        c1 = charLeast6 $ DB.shiftR n 18
    in (c1, c2, c3, c4)

int24LToBase64L :: [Int] -> String
int24LToBase64L [] = []
int24LToBase64L (n:[]) =
    let (c1, c2, c3, c4) = int24ToBase64 n
        padChar c = if c == 'A' then '=' else c
    in c1:c2:padChar c3:padChar c4:[]
int24LToBase64L (n:ns) =
    let (c1, c2, c3, c4) = int24ToBase64 n
    in c1:c2:c3:c4:int24LToBase64L ns

int12ToBase64 :: Int -> (Char, Char)
int12ToBase64 i = (high, low) where
    high = intToBase64 $ (DB..&.) 63 $ DB.shiftR i 6
    low = intToBase64 $ (DB..&.) 63 i

int12LToB64Str :: [Int] -> String
int12LToB64Str [] = []
int12LToB64Str (x:xs) =
    let (high, low) = int12ToBase64 x
    in high:low:(int12LToB64Str xs)

intToBase64 :: Int -> Char
intToBase64 i | i >= 0 && i <= 25 = DC.chr (65 + i)
              | i >= 26 && i <= 51 = DC.chr (97 + i - 26)
              | i >= 52 && i <= 61 = DC.chr (48 + i - 52)
              | i == 62 = '+'
              | i == 63 = '/'
              | otherwise = error "invalid integer digit in base 64"
