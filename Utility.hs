module Utility where

import qualified Data.Bits as DB
import qualified Data.ByteString as DBS
import qualified Data.Char as DC
import qualified Data.Text as DT
import Data.ByteString.Base64 (encode, decodeLenient)
import Data.ByteString.UTF8 (fromString, toString)
import GHC.Word (Word8)

import qualified AsciiOps as A
import qualified Base64Ops as B
import qualified HexOps as H
import qualified IntBaseUtils as IB

base64ToHex :: [Char] -> [Char]
base64ToHex = H.int12LToHexStr . B.base64LToInt12

base64ToInt8 :: [Char] -> [Int]
base64ToInt8 = IB.int12LToInt8L . B.base64LToInt12

base64ToAscii :: [Char] -> [Char]
base64ToAscii = toString. decodeLenient . fromString

hexToBase64 :: [Char] -> [Char]
hexToBase64 = B.int12LToB64Str . H.hexLToInt12

asciiToHex :: [Char] -> [Char]
asciiToHex = H.int8LToHexStr . (map A.asciiToInt8)

hexToAscii :: [Char] -> [Char]
hexToAscii = (map A.int8ToAscii) . H.hexLToInt8

asciiToBase64 :: [Char] -> [Char]
asciiToBase64 = toString . encode . fromString

xorAsHex :: [Char] -> [Char] -> [Char]
xorAsHex key xs = map H.fourBitToHex $ zipWith DB.xor (map DC.digitToInt (cycle key)) $ map DC.digitToInt xs

xorAsAscii :: Int -> Char -> Char
xorAsAscii key asc = H.int8ToPrintAscii $ DB.xor key (DC.ord asc)

-- given ascii key and ascii plaintext, return list of 8-bit integers from xoring the repeated key with plaintext
xorAsciiKey :: String -> String -> [Int]
xorAsciiKey key ascStr =
    zipWith DB.xor (map A.asciiToInt8 (cycle key)) $ map A.asciiToInt8 ascStr

-- same as above but return output as an ascii string
xorAsciiKeyStr :: String -> String -> String
xorAsciiKeyStr key ascStr = map A.int8ToAscii $ xorAsciiKey key ascStr

xorInt8WithString :: Int -> String -> String
xorInt8WithString key = map (xorAsAscii key)

-- given hex ciphertext and integer key, return the decrypted ascii string
xorInt8HexStr :: String -> Int -> String
xorInt8HexStr [] key = []
xorInt8HexStr (x1:x2:xs) key =
    let outChar = H.int8ToPrintAscii $ DB.xor (H.hexToInt8 x1 x2) key
    in outChar:xorInt8HexStr xs key
xorInt8HexStr _ key = error H.lengthErrMsg

tryKeysOnHexStr :: [Int] -> String -> [String]
tryKeysOnHexStr keys hexStr = map (xorInt8HexStr hexStr) keys

rateKeysBy :: (String -> Int) -> [Int] -> String -> [((Char, Int), String)]
rateKeysBy rateFctn keys cipherText =
    let appendKeyRating k xs = ((DC.chr k, rating), xs) where rating = rateFctn xs
    in zipWith appendKeyRating keys (tryKeysOnHexStr keys cipherText)

-- same as above but using Hex1CharDeciph syntax
deciphHexWithKey :: (String -> Int) -> Int -> String -> H.Hex1CharDeciph
deciphHexWithKey rateFctn key cipherText =
    let pt = xorInt8HexStr cipherText key
    in H.Hex1CharDeciph {
          H.cipherText=cipherText,
          H.keyInt=key,
          H.keyChar=H.int8ToPrintAscii key,
          H.plainText=pt,
          H.rating=rateFctn pt
}

deciphHexWithKeys :: (String -> Int) -> [Int] -> String -> [H.Hex1CharDeciph]
deciphHexWithKeys rateFctn keys cipherText =
    let oneDeciph = \x -> deciphHexWithKey rateFctn x cipherText
    in map oneDeciph keys

addkeyInfo ::
    [Int] -> String -> ([Int] -> String -> [String]) -> [(Int, String)]
addkeyInfo keys cipherText decryptFctn =
    let appendKey k xs = (k, xs)
    in zipWith appendKey keys (decryptFctn keys cipherText)

readLines xs = filter (\xs -> xs /= "") $ map DT.unpack $ DT.split (=='\n') $ DT.pack xs

