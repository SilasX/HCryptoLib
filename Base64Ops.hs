module Base64Ops where

import qualified Data.Bits as DB

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
    high = intToBase64 $ (DB..&.) 63 $ DB.shiftR i 6
    low = intToBase64 $ (DB..&.) 63 i

int12LToB64Str :: [Int] -> String
int12LToB64Str [] = []
int12LToB64Str (x:xs) =
    let (high, low) = int12ToBase64 x
    in high:low:(int12LToB64Str xs)

intToBase64 :: Int -> Char
intToBase64 0 = 'A'
intToBase64 1 = 'B'
intToBase64 2 = 'C'
intToBase64 3 = 'D'
intToBase64 4 = 'E'
intToBase64 5 = 'F'
intToBase64 6 = 'G'
intToBase64 7 = 'H'
intToBase64 8 = 'I'
intToBase64 9 = 'J'
intToBase64 10 = 'K'
intToBase64 11 = 'L'
intToBase64 12 = 'M'
intToBase64 13 = 'N'
intToBase64 14 = 'O'
intToBase64 15 = 'P'
intToBase64 16 = 'Q'
intToBase64 17 = 'R'
intToBase64 18 = 'S'
intToBase64 19 = 'T'
intToBase64 20 = 'U'
intToBase64 21 = 'V'
intToBase64 22 = 'W'
intToBase64 23 = 'X'
intToBase64 24 = 'Y'
intToBase64 25 = 'Z'
intToBase64 26 = 'a'
intToBase64 27 = 'b'
intToBase64 28 = 'c'
intToBase64 29 = 'd'
intToBase64 30 = 'e'
intToBase64 31 = 'f'
intToBase64 32 = 'g'
intToBase64 33 = 'h'
intToBase64 34 = 'i'
intToBase64 35 = 'j'
intToBase64 36 = 'k'
intToBase64 37 = 'l'
intToBase64 38 = 'm'
intToBase64 39 = 'n'
intToBase64 40 = 'o'
intToBase64 41 = 'p'
intToBase64 42 = 'q'
intToBase64 43 = 'r'
intToBase64 44 = 's'
intToBase64 45 = 't'
intToBase64 46 = 'u'
intToBase64 47 = 'v'
intToBase64 48 = 'w'
intToBase64 49 = 'x'
intToBase64 50 = 'y'
intToBase64 51 = 'z'
intToBase64 52 = '0'
intToBase64 53 = '1'
intToBase64 54 = '2'
intToBase64 55 = '3'
intToBase64 56 = '4'
intToBase64 57 = '5'
intToBase64 58 = '6'
intToBase64 59 = '7'
intToBase64 60 = '8'
intToBase64 61 = '9'
intToBase64 62 = '+'
intToBase64 63 = '/'
