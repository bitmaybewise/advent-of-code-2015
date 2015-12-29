module Lib
    ( answers
    ) where

import qualified Day1 as Day1
import qualified Day2 as Day2
import qualified Day3 as Day3
import qualified Day4 as Day4
import qualified Day5 as Day5
import qualified Day6 as Day6
import qualified Day8 as Day8
import qualified Day10 as Day10

answers :: IO ()
answers = do
    {-
    content <- readFile "day1.txt"
    Day1.answers content
    content <- readFile "day2.txt"
    Day2.answers content
    content <- readFile "day3.txt"
    Day3.answers content
    Day4.answers
    content <- readFile "day5.txt"
    Day5.answers content
    content <- readFile "day6.txt"
    Day6.answers content
    content <- readFile "day8.txt"
    Day8.answers content
    -}
    Day10.answers
