module Day5 
    ( answers
    ) where

import Data.List

notContainStrings :: String -> Bool
notContainStrings word = not $
                  "ab" `isInfixOf` word ||
                  "cd" `isInfixOf` word ||
                  "pq" `isInfixOf` word ||
                  "xy" `isInfixOf` word

isVowel :: Char -> Bool
isVowel char = (char == 'a') ||
               (char == 'e') ||
               (char == 'i') ||
               (char == 'o') ||
               (char == 'u')

containsThreeVowels :: String -> Bool
containsThreeVowels word = (length $ filter isVowel word) > 2

appearsTwice :: String -> Bool
appearsTwice []       = False
appearsTwice (_:[])   = False
appearsTwice (x:y:xs) = if x == y
                        then True
                        else appearsTwice (y:xs)

isNice :: String -> Bool
isNice word = let withVowels   = containsThreeVowels word
                  twiceLetters = appearsTwice word     
                  notContains  = notContainStrings word
              in withVowels && twiceLetters && notContains

niceStrings :: String -> Int
niceStrings content = length $ filter isNice (words content)

answers :: String -> IO ()
answers content = do
    putStrLn $ "day 5 part 1 = " ++ show (niceStrings content)
    putStrLn $ "day 5 part 2 = " ++ show (niceStrings content)
