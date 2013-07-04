module Table64 where

import qualified Data.Bits as DB
import qualified To64

base64ToInt :: Char -> Int
base64ToInt 'A' = 0
base64ToInt 'B' = 1
base64ToInt 'C' = 2
base64ToInt 'D' = 3
base64ToInt 'E' = 4
base64ToInt 'F' = 5
base64ToInt 'G' = 6
base64ToInt 'H' = 7
base64ToInt 'I' = 8
base64ToInt 'J' = 9
base64ToInt 'K' = 10
base64ToInt 'L' = 11
base64ToInt 'M' = 12
base64ToInt 'N' = 13
base64ToInt 'O' = 14
base64ToInt 'P' = 15
base64ToInt 'Q' = 16
base64ToInt 'R' = 17
base64ToInt 'S' = 18
base64ToInt 'T' = 19
base64ToInt 'U' = 20
base64ToInt 'V' = 21
base64ToInt 'W' = 22
base64ToInt 'X' = 23
base64ToInt 'Y' = 24
base64ToInt 'Z' = 25
base64ToInt 'a' = 26
base64ToInt 'b' = 27
base64ToInt 'c' = 28
base64ToInt 'd' = 29
base64ToInt 'e' = 30
base64ToInt 'f' = 31
base64ToInt 'g' = 32
base64ToInt 'h' = 33
base64ToInt 'i' = 34
base64ToInt 'j' = 35
base64ToInt 'k' = 36
base64ToInt 'l' = 37
base64ToInt 'm' = 38
base64ToInt 'n' = 39
base64ToInt 'o' = 40
base64ToInt 'p' = 41
base64ToInt 'q' = 42
base64ToInt 'r' = 43
base64ToInt 's' = 44
base64ToInt 't' = 45
base64ToInt 'u' = 46
base64ToInt 'v' = 47
base64ToInt 'w' = 48
base64ToInt 'x' = 49
base64ToInt 'y' = 50
base64ToInt 'z' = 51
base64ToInt '0' = 52
base64ToInt '1' = 53
base64ToInt '2' = 54
base64ToInt '3' = 55
base64ToInt '4' = 56
base64ToInt '5' = 57
base64ToInt '6' = 58
base64ToInt '7' = 59
base64ToInt '8' = 60
base64ToInt '9' = 61
base64ToInt '+' = 62
base64ToInt '/' = 63
base64ToInt _ = error "invalid base 64 char"

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
    high = To64.intToBase64 $ (DB..&.) 63 $ DB.shiftR i 6
    low = To64.intToBase64 $ (DB..&.) 63 i

int12LToB64Str :: [Int] -> String
int12LToB64Str [] = []
int12LToB64Str (x:xs) =
    let (high, low) = int12ToBase64 x
    in high:low:(int12LToB64Str xs)
