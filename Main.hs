module Main where

import Table64
import Numeric

testarg = "BA"
--main = print . fst . head $ readInt 64 isBase64 base64ToInt testarg
main = print $ readBase64ToInt testarg
