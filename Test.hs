module Test where

import qualified To64
import qualified Table64 as Tab64
import qualified HexOps as HexO

testHexStr = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
testBase64Str = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
main = do
    --let b64strInts = Tab64.base64LToInt12 testBase64Str
    --let hexInts = HexO.hexLToInt12 testHexStr
    let b64ToHex = HexO.int12LToHexStr $ Tab64.base64LToInt12 testBase64Str
    let hexToB64 = Tab64.int12LToB64Str $ HexO.hexLToInt12 testHexStr
    putStr "Conversion, base 64 to hex works? ... "
    putStrLn $ show $ b64ToHex == testHexStr
    putStr "Conversion, hex to base 64 works? ... "
    putStrLn $ show $ hexToB64 == testBase64Str
