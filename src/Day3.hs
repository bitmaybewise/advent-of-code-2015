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
moveUntilFinish (m:ms) location coord = let newCoord    = charToCoord m coord
                                            newLocation = Set.insert newCoord location
                                        in moveUntilFinish ms newLocation newCoord

moveUntilFinishWithRobot :: String -> Set.Set Coordinate -> Set.Set Coordinate -> Coordinate -> Coordinate -> Set.Set Coordinate
moveUntilFinishWithRobot [] santaLocation robotLocation _ _ = 
    Set.union santaLocation robotLocation
moveUntilFinishWithRobot (sm:[]) santaLocation robotLocation santaCoord _ = 
    let newSantaCoord = charToCoord sm santaCoord
        newSantaLocation = Set.insert newSantaCoord santaLocation
    in Set.union newSantaLocation robotLocation
moveUntilFinishWithRobot (sm:rm:ms) santaLocation robotLocation santaCoord robotCoord = 
    let newSantaCoord = charToCoord sm santaCoord
        newSantaLocation = Set.insert newSantaCoord santaLocation
        newRobotCoord = charToCoord rm robotCoord
        newRobotLocation = Set.insert newRobotCoord robotLocation
    in moveUntilFinishWithRobot ms newSantaLocation newRobotLocation newSantaCoord newRobotCoord

howManyHousesSantaVisited :: String -> Int
howManyHousesSantaVisited movements = let initialCoord = (0,0)
                                          initialHouse = Set.singleton initialCoord
                                          visits       = moveUntilFinish movements initialHouse initialCoord
                                      in Set.size visits

howManyHousesSantaAndRobotVisited :: String -> Int
howManyHousesSantaAndRobotVisited movements = let initialCoord = (0,0)
                                                  initialHouse = Set.singleton initialCoord
                                                  visits       = moveUntilFinishWithRobot movements initialHouse initialHouse initialCoord initialCoord
                                              in Set.size visits
answers :: String -> IO ()
answers movements = do
    putStrLn $ "day 3 part 1 = " ++ show (howManyHousesSantaVisited movements)
    putStrLn $ "day 3 part 2 = " ++ show (howManyHousesSantaAndRobotVisited movements)
