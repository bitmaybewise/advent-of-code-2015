{-# LANGUAGE OverloadedStrings #-}

module Day4
    ( answers
    ) where

import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy.Char8 as B

secretKey :: String
secretKey = "bgvyzdsv"

findInt :: [Int] -> String -> Int -> Int
findInt (x:xs) key zeros = let hashKey = secretKey ++ show x
                               hash    = B.pack . show $ md5 (B.pack hashKey)
                               zs      = B.pack . take zeros $ repeat '0'
                           in if B.isPrefixOf zs hash
                              then x
                              else findInt xs key zeros
mine :: Int -> Int
mine = findInt [100000..] secretKey

answers :: IO ()
answers = do
    putStrLn $ "day 4 part 1 = " ++ show (mine 5)
    putStrLn $ "day 4 part 2 = " ++ show (mine 6)
