-- File for extracting signal from noisy strings
-- Written by J. Yeaton for AOC 2016 (written in 2023)
-- Written on 3/13/23

-- Compile with "ghc --make SignalAndNoise"
-- Run with ./SignalAndNoise
-- GHCI with ghci SignalAndNoise.hs 

import System.IO
import Data.List
import Data.Function

main = do
    -- handle <- openFile "data_test.txt" ReadMode
    handle <- openFile "data.txt" ReadMode
    contents <- hGetContents handle
    -- let res = map (findMostCommonChar . group . sort) (foldl addLettersToCol ["","","","","",""] (lines contents))
    -- let res = map (findMostCommonChar . group . sort) (foldl addLettersToCol ["","","","","","","",""] (lines contents))
    let res = map (findLeastCommonChar . group . sort) (foldl addLettersToCol ["","","","","","","",""] (lines contents))
    print res
    hClose handle

addLettersToCol :: [String] -> String -> [String]
addLettersToCol (col:cols) (c:rest) = (col ++ [c]) : addLettersToCol cols rest
addLettersToCol noPW [] = noPW
addLettersToCol noPW _ = noPW

findMostCommonChar :: [String] -> Char
findMostCommonChar strs = head (maximumBy (compare `on` length) strs)

findLeastCommonChar :: [String] -> Char
findLeastCommonChar strs = head (minimumBy (compare `on` length) strs)