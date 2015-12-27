module Day8
    ( answers
    ) where

countChars :: String -> Int
countChars ('\\':'\\':ws)    = 1 + countChars ws
countChars ('\\':'"':ws)     = 1 + countChars ws
countChars ('\\':'x':_:_:ws) = 1 + countChars ws
countChars (_:ws)            = 1 + countChars ws
countChars []                = -2

countCharsEncoded :: String -> Int
countCharsEncoded ('\\':ws)    = 2 + countCharsEncoded ws
countCharsEncoded ('"':ws)     = 2 + countCharsEncoded ws
countCharsEncoded (_:ws)       = 1 + countCharsEncoded ws
countCharsEncoded []           = 2

literals :: [String] -> Int
literals words' = sum $ map length words'

total, total2 :: String -> Int

total strings = let words'   = words strings
                    inMemory = sum $ map countChars words'
                in (literals words') - inMemory

total2 strings = let words'  = words strings
                     encoded = sum $ map countCharsEncoded words'
                 in encoded - (literals words')

answers :: String -> IO ()
answers content = do
    putStrLn $ "day 8 part 1 = " ++ show (total content)
    putStrLn $ "day 8 part 2 = " ++ show (total2 content)
