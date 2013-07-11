{- Library for frequency analysis of possible ciphertexts -}

module Frequency where

import qualified Data.Char as DC
import qualified Data.List as DL
import qualified Data.Ord as DO
import Foreign.C.Types (CUChar)
import qualified Utility as U

perCharRate :: (Char -> Bool) -> Int -> [Char] -> Int
perCharRate clsr weight cphrtxt =
    DL.foldl' (\i c -> i + if clsr c then weight else 0) 0 cphrtxt

startWith :: [Char] -> [Char] -> Bool
startWith [] _ = True
startWith (pr:prs) [] = False
startWith (pr:prs) (x:xs) =
    if pr == x then startWith prs xs else False

-- given a list of "good" groups in english return a function that counts how many such groups are in the list, weighting by a given integer
--goodGroup :: [[Char]] -> Int -> [Char] -> Int
--goodGroup groups weight =
--    foldl (\i str -> (i + if or (map (flip startWith str) groups) then weight else 0)) 0
--
-- combinator for different rating functions
(=+=) :: ([Char] -> Int) -> ([Char] -> Int) -> [Char] -> Int
(=+=) rater1 rater2 =
    \xs -> rater1 xs + rater2 xs

-- first attempt at a rating function
freqCheck1 :: [Char] -> Int
freqCheck1 = perCharRate DC.isPrint 1
         =+= perCharRate DC.isSpace 1
         =+= perCharRate (flip elem "etaoin") 1

bestKeyHexMatch :: String -> ((Char, Int), String)
bestKeyHexMatch cipherText = DL.maximumBy (DO.comparing (snd . fst)) $ U.rateKeysBy freqCheck1 [0..255] cipherText

bestDecryption :: [((Char, Int), String)] -> ((Char, Int), String)
bestDecryption = DL.maximumBy (DO.comparing (snd . fst))
