module Day10
    ( answers
    ) where

input = "3113322113"

lookAndSay :: String -> String
lookAndSay []          = ""
lookAndSay [x]         = "1" ++ [x]
lookAndSay xs@(x:y:ys) = if x /= y
                         then "1" ++ [x] ++ lookAndSay (y:ys)
                         else let accEq []       _   = ("", "")
                                  accEq [a]      acc = (show acc ++ [a], "")
                                  accEq (a:b:as) acc = if a == b
                                                       then accEq (b:as) (acc + 1)
                                                       else (show acc ++ [a], (b:as))
                                  (translation, rest) = accEq xs 1
                              in translation ++ lookAndSay rest

lookAndSayAgain :: a -> String -> String
lookAndSayAgain _ newInput = lookAndSay newInput

lookAndSay40Times, lookAndSay50Times :: Int

lookAndSay40Times = length $ foldr lookAndSayAgain input [1..40]

lookAndSay50Times = length $ foldr lookAndSayAgain input [1..50]

answers :: IO ()
answers = do
    putStrLn $ "day 10 part 1 = " ++ (show lookAndSay40Times)
    putStrLn $ "day 10 part 2 = " ++ (show lookAndSay50Times)
