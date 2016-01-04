{-# LANGUAGE OverloadedStrings #-}

module Day12
    ( answers
    ) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
import Data.Scientific

parseJson :: String -> Maybe [Value]
parseJson content = decode (B.pack . head $ words content)

nonRed :: Value -> Bool
nonRed (String s) = s /= "red"
nonRed _          = True

parseValue, parseNonRedValue :: Value -> Integer

parseValue (Number n) = coefficient n
parseValue (Object o) = sum $ H.map parseValue o
parseValue (Array a)  = sum $ V.map parseValue a
parseValue _          = 0

parseNonRedValue (Number n) = coefficient n
parseNonRedValue (Object o) = let allNonRed = all nonRed $ H.elems o 
                              in if allNonRed
                                 then sum $ H.map parseNonRedValue o
                                 else 0
parseNonRedValue (Array a)  = sum $ V.map parseNonRedValue a
parseNonRedValue _          = 0

sumOfAllNumbers, sumOfAllNonRed :: String -> Integer

sumOfAllNumbers content = case (parseJson content) of
                              Just values -> sum $ map parseValue values
                              Nothing     -> 0

sumOfAllNonRed content = case (parseJson content) of
                              Just values -> sum $ map parseNonRedValue values
                              Nothing     -> 0

answers :: String -> IO ()
answers content = do
    putStrLn $ "day 12 part 1 = " ++ show (sumOfAllNumbers content)
    putStrLn $ "day 12 part 2 = " ++ show (sumOfAllNonRed content)
