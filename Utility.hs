module Utility where

import qualified Data.Bits as DB
import qualified HexOps as H

xorIntWithString :: Int -> String -> String
xorIntWithString key cipherText = let
																		decryptFunc = \x -> DB.xor x key
																	in
																		map (H.intToPrintAscii . decryptFunc) $ H.readHexToAscInt cipherText
