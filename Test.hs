module Test where

import qualified Data.Char as DC
import Test.HUnit (assertEqual)
import Test.QuickCheck (forAll, (==>))
import Test.QuickCheck.Test (quickCheck)
import Test.QuickCheck.Gen (Gen, choose, elements, listOf, suchThat)
import Test.QuickCheck.Property (Property, Testable)

import qualified Base64Ops as B
import qualified HexOps as HexO
import qualified Utility as U

-- example strings
testHexStr = "69b71d79f8218a39259a7a29aabb2dbafc31cb300108310518720928b30d38f411493515597619d76df8e7aefcf74fbf"
testBase64Str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890+/"

-- generate a list per listGen whose length is a multiple of n
lenMultN ::
    (Show a, Testable prop) => Int -> Gen [a] -> ([a] -> prop) -> Property
lenMultN n listGen = forAll (suchThat listGen (\x -> ((length x) `mod` n) == 0))

hexListGen = listOf $ elements $ ['0'..'9'] ++ ['a'..'f']
base64Gen = listOf $ elements $ ['+','/'] ++ ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']

-- the property that hex to int12 then back to hex is the identify function
prop_revFromHex =
    lenMultN 3 hexListGen $ \xs -> xs == (HexO.int12LToHexStr . HexO.hexLToInt12) xs

-- the property that base64 to int12 then back to base64 is the identify function
prop_revFromBase64 =
    lenMultN 2 base64Gen $ \xs -> xs == (B.int12LToB64Str . B.base64LToInt12) xs

prop_revFromBase64b =
    lenMultN 2 base64Gen $ \xs -> xs == (B.int24LToB64Str . B.base64LToInt24L) xs

prop_xorAsHexSelfReverse =
    lenMultN 1 hexListGen $ \key xs -> xs == U.xorAsHex key (U.xorAsHex key xs)

main = do
    quickCheck prop_revFromHex
    quickCheck prop_revFromBase64
    quickCheck prop_revFromBase64b
    assertEqual "Base 64 to hex should get expected result"
        testHexStr $ U.base64ToHex testBase64Str
    assertEqual "Hex to base 64 should get expected result"
        testBase64Str $ U.hexToBase64 testHexStr
