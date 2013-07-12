{- functinos for converting integers between bases 8, 12, 24
-}
module IntBaseUtils where

import qualified Data.Bits as DB

-- get least significant 8 bits (turned out to be common operation)
least8 :: Int -> Int
least8 = (DB..&.) 255
-- turn two (left, right) 12-bit integers into three 8-bit ints
-- high: most significant 8 bits of left
-- low: least significant 8 bits of right
-- mid: combine least 4 significant of left with most 4 significant of right
int12ToInt8 :: (Int, Int) -> (Int, Int, Int)
int12ToInt8 (left, right) =
    let high = DB.shiftR left 4
        mid = (+) (DB.shiftL (least8 left) 8) $ DB.shiftR right 8
        low = least8 right
    in (high, mid, low)

-- 8-bit to 24-bit conversion to facilitate translation between encodings
int8LToInt24L :: [Int] -> [Int]
int8LToInt24L [] = []
int8LToInt24L (x1:x2:x3:xs) =
    let low = x3
        mid = DB.shiftL x2 8
        high = DB.shiftL x1 16
    in (low + mid + high):int8LToInt24L xs
int8LToInt24L (x1:x2:[]) =
    let low = DB.shiftL x2 8
        high = DB.shiftL x1 16
    in (low + high):[]
int8LToInt24L (x1:[]) = (DB.shiftL x1 16):[]

int24ToInt8 :: Int -> (Int, Int, Int)
int24ToInt8 n =
    let low = least8 n
        mid = least8 $ DB.shiftR n 8
        high = least8 $ DB.shiftR n 16
    in (high, mid, low)

int24LToInt8L :: [Int] -> [Int]
int24LToInt8L [] = []
int24LToInt8L (x:xs) =
    let (high, mid, low) = int24ToInt8 x
    in high:mid:low:int24LToInt8L xs
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


