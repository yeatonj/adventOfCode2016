-- File for calculating bathroom codes
-- Written by J. Yeaton for AOC 2016 (written in 2023)
-- Written on 2/25/23

-- Compile with "ghc --make Triangles"
-- Run with ./Triangles
-- GHCI with ghci Triangles.hs 

import System.IO

main = do
    handle <- openFile "data.txt" ReadMode
    contents <- hGetContents handle
    -- let ct = checkTriangles (lines contents)
    let ct = checkTriangles (pt2Format (lines contents))
    print ct
    hClose handle

checkTriangles :: [String] -> Int
checkTriangles = foldl valid 0
    where valid cur tri = cur + fromEnum (validTriangle tri)

validTriangle :: String -> Bool
validTriangle t =
    let [a,b,c] = map (read :: String -> Int) (words t)
    in a + b > c && a + c > b && b + c > a

pt2Format :: [String] -> [String]
pt2Format a = repack (unpack 'a' [] [] [] a)

unpack :: Char -> [String] -> [String] -> [String] -> [String] -> ([String], [String], [String])
unpack _ as bs cs [] = (as, bs, cs)
unpack 'a' as bs cs (as':rem) = unpack 'b' (as ++ words as') bs cs rem
unpack 'b' as bs cs (bs':rem) = unpack 'c' as (bs ++ words bs') cs rem
unpack 'c' as bs cs (cs':rem) = unpack 'a' as bs (cs ++ words cs') rem

repack:: ([String], [String], [String]) -> [String]
repack([], [], []) = []
repack (a:roas, b:robs, c:rocs) = (a ++ " " ++ b ++ " " ++ c) : repack (roas, robs, rocs)