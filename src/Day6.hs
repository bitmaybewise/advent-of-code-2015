{--
--- Day 6: Probably a Fire Hazard ---

Because your neighbors keep defeating you in the holiday house decorating contest year after year, you've decided to deploy one million lights in a 1000x1000 grid.

Furthermore, because you've been especially nice this year, Santa has mailed you instructions on how to display the ideal lighting configuration.

Lights in your grid are numbered from 0 to 999 in each direction; the lights at each corner are at 0,0, 0,999, 999,999, and 999,0. The instructions include whether to turn on, turn off, or toggle various inclusive ranges given as coordinate pairs. Each coordinate pair represents opposite corners of a rectangle, inclusive; a coordinate pair like 0,0 through 2,2 therefore refers to 9 lights in a 3x3 square. The lights all start turned off.

To defeat your neighbors this year, all you have to do is set up your lights by doing the instructions Santa sent you in order.

For example:

    turn on 0,0 through 999,999 would turn on (or leave on) every light.
    toggle 0,0 through 999,0 would toggle the first line of 1000 lights, turning off the ones that were on, and turning on the ones that were off.
    turn off 499,499 through 500,500 would turn off (or leave off) the middle four lights.

After following the instructions, how many lights are lit?

--- Part Two ---

You just finish implementing your winning light pattern when you realize you mistranslated Santa's message from Ancient Nordic Elvish.

The light grid you bought actually has individual brightness controls; each light can have a brightness of zero or more. The lights all start at zero.

The phrase turn on actually means that you should increase the brightness of those lights by 1.

The phrase turn off actually means that you should decrease the brightness of those lights by 1, to a minimum of zero.

The phrase toggle actually means that you should increase the brightness of those lights by 2.

What is the total brightness of all lights combined after following Santa's instructions?

For example:

    turn on 0,0 through 0,0 would increase the total brightness by 1.
    toggle 0,0 through 999,999 would increase the total brightness by 2000000.
--}

{-# LANGUAGE OverloadedStrings #-}

module Day6
    ( answers
    ) where

import Data.Attoparsec.Text
import qualified Data.Text as T
import Control.Applicative
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.List as L

data Action = TurnOff | TurnOn | Toggle deriving Show

type Coordinate = (Int, Int)

type Lights = M.Map Coordinate Bool

type Brightness = M.Map Coordinate Int

data Instruction = Instruction Action Coordinate Coordinate deriving Show

parseAction :: Parser Action
parseAction = do
        (string "turn on"  >> return TurnOn)
    <|> (string "turn off" >> return TurnOff)
    <|> (string "toggle"   >> return Toggle)

parseCoordinate :: Parser Coordinate
parseCoordinate = do
    x <- decimal
    char ','
    y <- decimal
    return (x, y)

parseInstruction :: Parser Instruction
parseInstruction = do
    action <- parseAction
    char ' '
    start <- parseCoordinate
    string " through "
    end <- parseCoordinate
    return $ Instruction action start end

executeAction :: Action -> Lights -> Coordinate -> Lights
executeAction TurnOff lights coord = M.insert coord False lights
executeAction TurnOn  lights coord = M.insert coord True lights
executeAction Toggle  lights coord = let maybeValue = M.lookup coord lights
                                         value      = not $ fromMaybe False maybeValue
                                     in M.insert coord value lights

changeLights :: Lights -> Instruction -> Lights
changeLights lights instruction = let (Instruction action (x1, y1) (x2, y2)) = instruction
                                      coordinates = [(x, y) | x <- [x1..x2], y <- [y1..y2]]
                                  in L.foldl' (executeAction action) lights coordinates

parseAllInstructions = parseOnly (many $ parseInstruction <* endOfLine)

howManyLightsAreLit :: String -> Int
howManyLightsAreLit lines = let instructions = parseAllInstructions $ T.pack lines
                            in case instructions of
                                   Right xs -> length $ M.filter id $ L.foldl' changeLights M.empty xs
                                   Left _   -> 0
        
updateBrightness :: Action -> Brightness -> Coordinate -> Brightness
updateBrightness TurnOff lights coord = let maybeValue = M.lookup coord lights
                                            value      = fromMaybe 0 maybeValue
                                        in if value > 0
                                           then M.insert coord (value - 1) lights
                                           else M.insert coord 0 lights
updateBrightness TurnOn  lights coord = let maybeValue = M.lookup coord lights
                                            value      = fromMaybe 0 maybeValue
                                        in M.insert coord (value + 1) lights
updateBrightness Toggle  lights coord = let maybeValue = M.lookup coord lights
                                            value      = fromMaybe 0 maybeValue
                                        in M.insert coord (value + 2) lights

changeBrightness :: Brightness -> Instruction -> Brightness
changeBrightness lights instruction = let (Instruction action (x1, y1) (x2, y2)) = instruction
                                          coordinates = [(x, y) | x <- [x1..x2], y <- [y1..y2]]
                                      in L.foldl' (updateBrightness action) lights coordinates

totalBrightness :: String -> Int
totalBrightness lines = let instructions = parseAllInstructions $ T.pack lines
                        in case instructions of
                               Right xs -> M.foldl (+) 0 $ L.foldl' changeBrightness M.empty xs
                               Left _   -> 0

answers :: String -> IO ()
answers content = do
    putStrLn $ "day 6 part 1 = " ++ show (howManyLightsAreLit content)
    putStrLn $ "day 6 part 2 = " ++ show (totalBrightness content)
