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
    -- let dist = calcDist contents True -- For part 1
    let dist = calcDist contents False -- For part 2
    print dist
    hClose handle

-- String Bool -> Integral
-- Function to calculate distance traveled given a string of directions, false prevents overlaps
calcDist ::  String -> Bool -> Integer
calcDist ds overlaps =
    let (x, y, overlapped, visited) = simFullWalk (words ds) 'N' (0,0, False, Set.singleton (0, 0)) overlaps
    in abs x + abs y

-- [String] Char (Integral, Integral, Bool) Set(Integer, Integer) Bool -> (Integral, Integral, Bool)
-- Function to simulate a full walk based on an array of directions, current pos, and direction, 
-- returning final position. Bool overlap sets whether overlaps are allowed
simFullWalk :: [String] -> Char -> (Integer, Integer, Bool, Set.Set(Integer, Integer)) -> Bool -> (Integer, Integer, Bool, Set.Set(Integer, Integer))
simFullWalk [] _ loc _ = loc
simFullWalk _ _ (x, y, True, visited) False = (x, y, True, visited) -- If we aren't allowing overlaps + soln found, return it
simFullWalk (d:ds) dir loc overlap = 
    let (newDir, moveDist) = newMove d dir
    in simFullWalk ds newDir (visitPoints loc overlap moveDist newDir) overlap

-- (Integral, Integral, Bool) Bool Integral Char Set(Integer, Integer) -> (Integral, Integral, Bool, Set(Integer, Integer))
-- Visits points along a direction
visitPoints :: (Integer, Integer, Bool, Set.Set (Integer, Integer)) -> Bool -> Integer -> Char -> (Integer, Integer, Bool, Set.Set (Integer, Integer))
visitPoints (x,y,True,visited) False _ _ = (x,y,True,visited) -- Overlap when overlap is not allowed
visitPoints (x,y,f,visited) overlap 0 dir = (x,y,f,visited) -- Finished our move
visitPoints (x,y,f,visited) overlap dRem 'N' =
    let (xNew, yNew, prevVisited, newVisited) = visitPoint (x, y + 1) visited
    in visitPoints (xNew, yNew, prevVisited, newVisited) overlap (dRem - 1) 'N'
visitPoints (x,y,f, visited) overlap dRem 'W' =
    let (xNew, yNew, prevVisited, newVisited) = visitPoint (x - 1, y) visited
    in visitPoints (xNew, yNew, prevVisited, newVisited) overlap (dRem - 1) 'W'
visitPoints (x,y,f, visited) overlap dRem 'E' =
    let (xNew, yNew, prevVisited, newVisited) = visitPoint (x + 1, y) visited
    in visitPoints (xNew, yNew, prevVisited, newVisited) overlap (dRem - 1) 'E'
visitPoints (x,y,f, visited) overlap dRem 'S' =
    let (xNew, yNew, prevVisited, newVisited) = visitPoint (x, y - 1) visited
    in visitPoints (xNew, yNew, prevVisited, newVisited) overlap (dRem - 1) 'S'
visitPoints _ _ _ _ = error "Incorrect direction passed to function"

-- (Integral, Integral) Set(Integer, Integer) -> (Integral, Integral, Bool, Set(Integer, Integer))
-- Checks to see if a point has already been visited, adds to visited set if not. Sets flag if so
visitPoint :: (Integer, Integer) -> Set.Set(Integer, Integer) -> (Integer, Integer, Bool, Set.Set(Integer, Integer))
visitPoint (newX, newY) visited =
    if Set.member (newX, newY) visited
        then (newX, newY, True, visited)
    else let newVisited = Set.insert (newX, newY) visited
    in (newX, newY, False, newVisited)


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