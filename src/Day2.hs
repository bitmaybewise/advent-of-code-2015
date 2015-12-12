module Day2
    ( answers
    ) where

import qualified Data.List as List

data Dimension = Dimension { l :: Int, 
                             w  :: Int, 
                             h :: Int } deriving Show

data Sides = Sides { s1 :: Int, 
                     s2 :: Int, 
                     s3 :: Int } deriving Show

getDimension :: String -> Dimension
getDimension xs = let (length', rest) = span (/= 'x') xs
                      (width', rest') = span (/= 'x') (tail rest)
                      height' = tail rest'
                   in Dimension { l = read length', w = read width', h = read height' }

getDimensions :: String -> [Dimension]
getDimensions boxes = map getDimension $ words boxes

dimensionSides :: Dimension -> Sides
dimensionSides dimension = Sides { s1 = l dimension * w dimension, 
                                   s2 = w dimension * h dimension, 
                                   s3 = h dimension * l dimension }

surfaceArea :: Sides -> Int
surfaceArea sides = 2 * (s1 sides + s2 sides + s3 sides)

smallestArea :: Sides -> Int
smallestArea sides = let Sides { s1 = s1', s2 = s2', s3 = s3' } = sides
                     in s1' `min` s2' `min` s3'

squareFeet :: Sides -> Int
squareFeet sides = (surfaceArea sides) + (smallestArea sides)

totalSquareFeet :: String -> Int
totalSquareFeet boxes = sum $ map (squareFeet . dimensionSides) $ getDimensions boxes

ribbonToWrap :: Dimension -> Int
ribbonToWrap dimension = let values = [l dimension, w dimension, h dimension]
                             ordered = List.sort values
                         in 2 * (sum $ take 2 ordered)

ribbonForTheBow :: Dimension -> Int
ribbonForTheBow dimension = l dimension * w dimension * h dimension

feetOfRibbon :: Dimension -> Int
feetOfRibbon dimension = ribbonForTheBow dimension + ribbonToWrap dimension

totalFeetOfRibbon :: String -> Int
totalFeetOfRibbon boxes = sum $ map feetOfRibbon $ getDimensions boxes

answers :: String -> IO ()
answers boxes = do
    putStrLn $ "day 2 part 1 = " ++ show (totalSquareFeet boxes)
    putStrLn $ "day 2 part 2 = " ++ show (totalFeetOfRibbon boxes)
