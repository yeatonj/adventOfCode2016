-- File for calculating taxicab geometry
-- Written by J. Yeaton for AOC 2016 (written in 2023)
-- Written on 2/22/23

-- Compile with "ghc --make noTimeForTaxicab"
-- Run with ./noTimeForTaxicab

import System.IO
import qualified Data.Set as Set

main = do
    handle <- openFile "data.txt" ReadMode
    contents <- hGetContents handle
    --putStr contents
    let dist = calcDist contents
    print dist
    hClose handle

-- String -> Integral
-- Function to calculate distance traveled given a string of directions
calcDist ::  String -> Integer
calcDist ds =
    let (x, y) = simFullWalk (words ds) 'N' (0,0)
    in abs x + abs y

-- [String] Char (Integral, Integral) -> (Integral, Integral)
-- Function to simulate a full walk based on an array of directions, current pos, and direction, 
-- returning final position
simFullWalk :: [String] -> Char -> (Integer, Integer) -> (Integer, Integer)
simFullWalk [] _ loc = loc
simFullWalk (d:ds) dir loc = 
    let (newDir, moveDist) = newMove d dir
    in simFullWalk ds newDir (simWalk newDir moveDist loc)

-- String String -> (String, Integral)
-- Function to return a new direction and dist to move based on current direction and next directions
newMove :: String -> Char -> (Char, Integer)
newMove (turn:num) curDir = 
    let move = if last num == ',' then read (take (length num - 1) num) :: Integer else read num :: Integer
    in (newDirection curDir turn, move)

-- Char Char -> Char
-- Takes an old direction and turn, returns the new direction
newDirection :: Char -> Char -> Char
newDirection 'N' 'R' = 'E'
newDirection 'N' 'L' = 'W'
newDirection 'E' 'R' = 'S'
newDirection 'E' 'L' = 'N'
newDirection 'S' 'R' = 'W'
newDirection 'S' 'L' = 'E'
newDirection 'W' 'R' = 'N'
newDirection 'W' 'L' = 'S'


-- Char (Integral, Integral) -> (Integral, Integral)
-- Function to calculate new position based on current direction and distance to move
simWalk :: (Integral a) => Char -> a -> (a, a) -> (a, a)
simWalk 'N' d (x,y) = (x, y + d)
simWalk 'W' d (x,y) = (x - d, y)
simWalk 'E' d (x,y) = (x + d, y)
simWalk 'S' d (x,y) = (x, y - d)
simWalk _ _ _ = error "Incorrect direction passed to function"