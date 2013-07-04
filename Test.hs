module Test where

import qualified To64
import qualified Table64 as Tab64
import qualified HexOps as HexO

testHexStr = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
testBase64Str = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
main = do
    -- test that base64 -> hex works
    --putStr "Does base64 to hex work? \t"
    let b64ConvCheck = HexO.intLToHexStr $ map Tab64.base64ToInt testBase64Str
    let testResult1 = b64ConvCheck == testHexStr
    --putStrLn $ show testResult1
    putStrLn testHexStr
    putStrLn b64ConvCheck
