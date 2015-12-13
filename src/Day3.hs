module Day3
    ( answers
    ) where

import qualified Data.Set as Set

type Coordinate = (Int, Int)

charToCoord :: Char -> Coordinate -> Coordinate
charToCoord '^' (x, y) = (x, y + 1)
charToCoord 'v' (x, y) = (x, y - 1)
charToCoord '>' (x, y) = (x + 1, y)
charToCoord '<' (x, y) = (x - 1, y)
charToCoord _ coord    = coord

moveUntilFinish :: String -> Set.Set Coordinate -> Coordinate -> Set.Set Coordinate
moveUntilFinish [] location _         = location
moveUntilFinish (m:ms) location coord = let newCoord = charToCoord m coord
                                            newLocation = Set.insert newCoord location
                                        in moveUntilFinish ms newLocation newCoord

howManyHouses :: String -> Int
howManyHouses movements = let initialCoord = (0,0)
                              initialHouse = Set.singleton initialCoord
                              finalHouse = moveUntilFinish movements initialHouse initialCoord
                          in Set.size finalHouse

answers :: String -> IO ()
answers movements = do
    putStrLn $ "day 3 part 1 = " ++ show (howManyHouses movements)
    putStrLn $ "day 3 part 2 = "
