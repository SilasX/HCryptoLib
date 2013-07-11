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
              | otherwise = error "invalid base 64 char"

isBase64 :: Char -> Bool
isBase64 a = or
    [(a >= 'A') && (a <= 'Z'),
     (a >= 'a') && (a <= 'z'),
     (a >= '0') && (a <= '9'),
     (a == '+'),
     (a == '/')]

readBase64ToInt :: String -> [Int]
readBase64ToInt [] = []
readBase64ToInt (x:xs) = (base64ToInt x) : readBase64ToInt xs

base64LToInt12 :: String -> [Int]
base64LToInt12 [] = []
base64LToInt12 (x1:x2:xs) =
    (64*(base64ToInt x1) + base64ToInt x2):(base64LToInt12 xs)
base64LToInt12 _ = error "not appropriate length base 64 string"

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
