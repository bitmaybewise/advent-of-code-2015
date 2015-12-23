module Day8
    ( answers
    ) where

countChars :: String -> Int
countChars ('\\':'\\':ws)    = 1 + countChars ws
countChars ('\\':'\"':ws)    = 1 + countChars ws
countChars ('\\':'x':_:_:ws) = 1 + countChars ws
countChars (_:ws)            = 1 + countChars ws
countChars []                = -2

total :: String -> Int
total strings = let words'   = words strings
                    literals = sum $ map length words'
                    inMemory = sum $ map countChars words'
                in literals - inMemory

answers :: String -> IO ()
answers content = do
    putStrLn $ "day 8 part 1 = " ++ show (total content)
