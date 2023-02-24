-- File for calculating bathroom codes
-- Written by J. Yeaton for AOC 2016 (written in 2023)
-- Written on 2/24/23

-- Compile with "ghc --make BathroomSecurity"
-- Run with ./BathroomSecurity
-- GHCI with ghci BathroomSecurity.hs 

import System.IO
    ( hClose, hGetContents, openFile, IOMode(ReadMode) )

main = do
    handle <- openFile "data.txt" ReadMode
    contents <- hGetContents handle
    let code = findCode contents
    print code
    hClose handle

-- Consumes a list of instructions, produces a code (string)
findCode :: String -> String
findCode ins = performIns ins '5' ""

-- Accepts a string of instructions, the current key, and the current code
-- produces the code from these instructions
performIns :: String -> Char -> String -> String
performIns [] curKey curCode = 
    curCode ++ [curKey]
performIns ('\n':remDirs) curKey curCode = 
    performIns remDirs curKey (curCode ++ [curKey])
-- performIns (cmd:remDirs) curKey curCode = 
--     performIns remDirs (moveKey cmd curKey) curCode -- uncomment for pt1
performIns (cmd:remDirs) curKey curCode = 
    performIns remDirs (moveKey2 cmd curKey) curCode -- uncomment for pt2

-- Accepts the current key and a direction, returns the new key
moveKey :: Char -> Char -> Char
moveKey 'R' '1' = '2'
moveKey 'D' '1' = '4'
moveKey 'L' '2' = '1'
moveKey 'D' '2' = '5'
moveKey 'R' '2' = '3'
moveKey 'L' '3' = '2'
moveKey 'D' '3' = '6'
moveKey 'R' '4' = '5'
moveKey 'U' '4' = '1'
moveKey 'D' '4' = '7'
moveKey 'U' '5' = '2'
moveKey 'R' '5' = '6'
moveKey 'D' '5' = '8'
moveKey 'L' '5' = '4'
moveKey 'L' '6' = '5'
moveKey 'U' '6' = '3'
moveKey 'D' '6' = '9'
moveKey 'R' '7' = '8'
moveKey 'U' '7' = '4'
moveKey 'U' '8' = '5'
moveKey 'R' '8' = '9'
moveKey 'L' '8' = '7'
moveKey 'U' '9' = '6'
moveKey 'L' '9' = '8'
moveKey _ key = key

-- Same as above, for pt2
moveKey2 :: Char -> Char -> Char
moveKey2 'D' '1' = '3'
moveKey2 'R' '2' = '3'
moveKey2 'D' '2' = '6'
moveKey2 'L' '3' = '2'
moveKey2 'D' '3' = '7'
moveKey2 'U' '3' = '1'
moveKey2 'R' '3' = '4'
moveKey2 'L' '4' = '3'
moveKey2 'D' '4' = '8'
moveKey2 'R' '5' = '6'
moveKey2 'L' '6' = '5'
moveKey2 'U' '6' = '2'
moveKey2 'D' '6' = 'A'
moveKey2 'R' '6' = '7'
moveKey2 'R' '7' = '8'
moveKey2 'U' '7' = '3'
moveKey2 'D' '7' = 'B'
moveKey2 'L' '7' = '6'
moveKey2 'U' '8' = '4'
moveKey2 'R' '8' = '9'
moveKey2 'L' '8' = '7'
moveKey2 'D' '8' = 'C'
moveKey2 'L' '9' = '8'
moveKey2 'U' 'A' = '6'
moveKey2 'R' 'A' = 'B'
moveKey2 'U' 'B' = '7'
moveKey2 'R' 'B' = 'C'
moveKey2 'L' 'B' = 'A'
moveKey2 'D' 'B' = 'D'
moveKey2 'U' 'C' = '8'
moveKey2 'L' 'C' = 'B'
moveKey2 'U' 'D' = 'B'
moveKey2 _ key = key