module Lib
    ( answers
    ) where

import qualified Day1 as Day1
import qualified Day2 as Day2

answers :: IO ()
answers = do
    content <- readFile "day1.txt"
    Day1.answers content
    content <- readFile "day2.txt"
    Day2.answers content
