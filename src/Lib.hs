module Lib
    ( answers
    ) where

day1 :: String -> Int
day1 content = sum $ map toNum content
               where
                  toNum '(' = 1
                  toNum ')' = -1
                  toNum _   = 0

answers :: IO ()
answers = do
    content <- readFile "day1.txt"
    putStrLn $ "day 1 = " ++ show (day1 content)
