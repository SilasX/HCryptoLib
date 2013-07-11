module AsciiOps where

import qualified Data.Bits as DB
import qualified Data.Char as DC

asciiToInt8 :: Char -> Int
asciiToInt8 = DC.ord

int8ToAscii :: Int -> Char
int8ToAscii = DC.chr
