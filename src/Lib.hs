module Lib
    ( answers
    ) where

toNum :: Char -> Int
toNum '(' = 1
toNum ')' = -1
toNum _   = 0

day1Part1, day1Part2 :: String -> Int

day1Part1 content = sum $ map toNum content

day1Part2 content = findIndex content 0 0
                    where findIndex _ index (-1)     = index
                          findIndex (x:xs) index acc = findIndex xs (index + 1) (acc + toNum x)

answers :: IO ()
answers = do
    content <- readFile "day1.txt"
    putStrLn $ "day 1 part 1 = " ++ show (day1Part1 content)
    putStrLn $ "day 1 part 2 = " ++ show (day1Part2 content)
