module Test where

import qualified Data.Char as DC
import Test.QuickCheck (forAll, (==>))
import Test.QuickCheck.Test (quickCheck)
import Test.QuickCheck.Gen (Gen, choose, elements, listOf, suchThat)

import qualified To64
import qualified Table64 as Tab64
import qualified HexOps as HexO

-- generate a list per listGen whose length is a multiple of n
lenMultN n listGen = forAll (suchThat listGen (\x -> ((length x) `mod` n) == 0))

hexListGen = listOf $ elements $ ['0'..'9'] ++ ['a'..'f']
base64Gen = listOf $ elements $ ['+','/'] ++ ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']

-- the property that hex to int12 then back to hex is the identify function
prop_revFromHex =
    lenMultN 3 hexListGen $ \xs -> xs == (HexO.int12LToHexStr . HexO.hexLToInt12) xs

-- the property that base64 to int12 then back to base64 is the identify function
prop_revFromBase64 =
    lenMultN 2 base64Gen $ \xs -> xs == (Tab64.int12LToB64Str . Tab64.base64LToInt12) xs

main = do
    quickCheck prop_revFromHex
    quickCheck prop_revFromBase64
