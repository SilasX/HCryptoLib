module Utility where

import qualified Data.Bits as DB
import qualified HexOps as H
import qualified Table64 as Tab64

base64ToHex :: [Char] -> [Char]
base64ToHex = H.int12LToHexStr . Tab64.base64LToInt12

hexToBase64 :: [Char] -> [Char]
hexToBase64 = Tab64.int12LToB64Str . H.hexLToInt12

xorIntWithString :: Int -> String -> String
xorIntWithString key cipherText =
    let decryptFunc = \x -> DB.xor x key
    in map (H.intToPrintAscii . decryptFunc) $ H.readHexToAscInt cipherText
