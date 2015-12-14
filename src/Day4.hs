{-# LANGUAGE OverloadedStrings #-}

module Day4
    ( answers
    ) where

import Data.Digest.Pure.MD5
import Data.ByteString.Lazy.Char8 hiding (putStrLn, foldl, take)

secretKey :: String
secretKey = "bgvyzdsv"

checkAll :: [Char] -> Char -> Bool
checkAll [] _        = True
checkAll (x:xs) char = if x == char
                       then checkAll xs char
                       else False

startsWithZeros :: String -> Bool
startsWithZeros hash = let initials = take 5 hash
                       in checkAll initials '0'

findInt :: [Int] -> String -> Int
findInt (x:xs) key = let hashKey = secretKey ++ show x
                         hash    = show $ md5 (pack hashKey)
                     in if startsWithZeros hash
                        then x
                        else findInt xs key
mine :: Int
mine = findInt [100000..] secretKey

answers :: IO ()
answers = do
    putStrLn $ "day 4 part 1 = " ++ show mine
