module Main where

import Numeric

import qualified Frequency as F
import qualified Table64 as Tab64
import qualified Utility as U

main = do
    textIn <- readFile "ciphertext.txt"
    let cipherList = U.readLines textIn
    let bestDecrypts = map F.bestKeyHexMatch cipherList
    let out = F.bestDecryption bestDecrypts
    putStrLn $ show out
