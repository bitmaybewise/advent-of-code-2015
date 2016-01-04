{-# LANGUAGE OverloadedStrings #-}

module Day12
    ( answers
    ) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V

parseValue (Number n) = n
parseValue (Object o) = sum $ H.map parseValue o
parseValue (Array a)  = sum $ V.map parseValue a
parseValue _          = 0

sumOfAllNumbers content = let line = B.pack . head $ words content
                              json = decode line :: Maybe [Value]
                          in case json of
                                 Just values -> sum $ map parseValue values
                                 Nothing     -> 0

answers :: String -> IO ()
answers content = do
    putStrLn $ "day 12 part 1 = " ++ show (sumOfAllNumbers content)
