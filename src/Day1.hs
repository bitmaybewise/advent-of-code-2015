module Day1 
    ( answers
    ) where

toNum :: Char -> Int
toNum '(' = 1
toNum ')' = -1
toNum _   = 0

part1, part2 :: String -> Int

part1 chars = sum $ map toNum chars

part2 chars = findIndex chars 0 0
              where findIndex _ index (-1)     = index
                    findIndex (x:xs) index acc = findIndex xs (index + 1) (acc + toNum x)

answers :: String -> IO ()
answers chars = do
    putStrLn $ "day 1 part 1 = " ++ show (part1 chars)
    putStrLn $ "day 1 part 2 = " ++ show (part2 chars)
