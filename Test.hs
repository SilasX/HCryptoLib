module Test where

import qualified To64
import qualified Table64 as Tab64
import qualified HexOps as HexO

testHexStr = "69b71d79f8218a39259a7a29aabb2dbafc31cb300108310518720928b30d38f411493515597619d76df8e7aefcf74fbf"
testBase64Str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890+/"
main = do
    --let b64strInts = Tab64.base64LToInt12 testBase64Str
    --let hexInts = HexO.hexLToInt12 testHexStr
    let b64ToHex = HexO.int12LToHexStr $ Tab64.base64LToInt12 testBase64Str
    let hexToB64 = Tab64.int12LToB64Str $ HexO.hexLToInt12 testHexStr
    putStr "Conversion, base 64 to hex works? ... "
    putStrLn $ show $ b64ToHex == testHexStr
    putStr "Conversion, hex to base 64 works? ... "
    putStrLn $ show $ hexToB64 == testBase64Str
