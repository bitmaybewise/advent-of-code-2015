{--
--- Day 5: Doesn't He Have Intern-Elves For This? ---

Santa needs help figuring out which strings in his text file are naughty or nice.

A nice string is one with all of the following properties:

    It contains at least three vowels (aeiou only), like aei, xazegov, or aeiouaeiouaeiou.
    It contains at least one letter that appears twice in a row, like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
    It does not contain the strings ab, cd, pq, or xy, even if they are part of one of the other requirements.

For example:

    ugknbfddgicrmopn is nice because it has at least three vowels (u...i...o...), a double letter (...dd...), and none of the disallowed substrings.
    aaa is nice because it has at least three vowels and a double letter, even though the letters used by different rules overlap.
    jchzalrnumimnmhp is naughty because it has no double letter.
    haegwjzuvuyypxyu is naughty because it contains the string xy.
    dvszwmarrgswjxmb is naughty because it contains only one vowel.

How many strings are nice?

--- Part Two ---

Realizing the error of his ways, Santa has switched to a better model of determining whether a string is naughty or nice. None of the old rules apply, as they are all clearly ridiculous.

Now, a nice string is one with all of the following properties:

    It contains a pair of any two letters that appears at least twice in the string without overlapping, like xyxy (xy) or aabcdefgaa (aa), but not like aaa (aa, but it overlaps).
    It contains at least one letter which repeats with exactly one letter between them, like xyx, abcdefeghi (efe), or even aaa.

For example:

    qjhvhtzxzqqjkmpb is nice because is has a pair that appears twice (qj) and a letter that repeats with exactly one letter between them (zxz).
    xxyxx is nice because it has a pair that appears twice and a letter that repeats with one between, even though the letters used by each rule overlap.
    uurcxstgmygtbstg is naughty because it has a pair (tg) but no repeat with a single letter between them.
    ieodomkazucvgmuy is naughty because it has a repeating letter with one between (odo), but no pair that appears twice.

How many strings are nice under these new rules?
--}

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

twiceWithoutOverlapping :: String -> Bool
twiceWithoutOverlapping word = if (length word) < 4
                               then False
                               else let (x:y:z:xs) = word
                                        appearsTwice = [x, y] `isInfixOf` (z:xs)
                                        dontOverlaps = not $ [x, x, x] `isInfixOf` word
                                    in if appearsTwice && dontOverlaps
                                       then True
                                       else twiceWithoutOverlapping (y:z:xs)

repeatsBetween :: String -> Bool
repeatsBetween word = if (length word) < 3
                      then False
                      else let (x:y:z:xs) = word
                           in if (x == z) && (x /= y)
                              then True
                              else repeatsBetween (y:z:xs)

isNewNice :: String -> Bool
isNewNice word = let twice   = twiceWithoutOverlapping word
                     repeats = repeatsBetween word
                 in twice && repeats


newNiceStrings :: String -> Int
newNiceStrings content = length $ filter isNewNice (words content)

answers :: String -> IO ()
answers content = do
    putStrLn $ "day 5 part 1 = " ++ show (niceStrings content)
    putStrLn $ "day 5 part 2 = " ++ show (newNiceStrings content)
