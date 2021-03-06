module Main where

import Numeric

import qualified Frequency as F
import qualified Utility as U

main = do
    textIn <- readFile "ciphertexts/ciphertext.txt"
    let cipherList = U.readLines textIn
    let bestDecrypts = map F.bestKeyHexMatch cipherList
    let out = F.bestDecryption bestDecrypts
    putStrLn $ show out
