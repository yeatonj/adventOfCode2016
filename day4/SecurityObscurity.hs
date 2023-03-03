-- File for validating checksums
-- Written by J. Yeaton for AOC 2016 (written in 2023)
-- Written on 2/26/23

-- Compile with "ghc --make SecurityObscurity"
-- Run with ./SecurityObscurity
-- GHCI with ghci SecurityObscurity.hs 

import Data.List
import System.IO
import Data.Char

main = do
    handle <- openFile "data.txt" ReadMode
    contents <- hGetContents handle
    -- let ct = foldl (\i line -> i + idResult line) 0 (lines contents)
    -- print ct
    let res = findRoomMatch (map decryptRoomFull (lines contents)) "north"
    print res
    hClose handle

idResult :: String -> Int
idResult s =
    let actualCksum = findChecksum s
        (id, assertCksum) = extractIDsum s
    in
        if actualCksum == assertCksum then id
        else 0

extractIDsum :: String -> (Int, String)
extractIDsum s_in =
    let idsum = last (splitStr '-' s_in)
        [id,cksum] = splitStr '[' idsum
    in (read id :: Int, take 5 cksum)

findChecksum :: String -> String
findChecksum s_in =
    let topFive = take 5 (sortBy sortPairs (countAllLetters (splitStr '-' s_in) []))
        extractFirst (c, ct) = c
    in map extractFirst topFive

countAllLetters :: [String] -> [(Char,Integer)] -> [(Char, Integer)]
countAllLetters [c_sum] counts = counts
countAllLetters (str:strs) counts = countAllLetters strs (countLetters str counts)

countLetters :: [Char] -> [(Char, Integer)] -> [(Char, Integer)]
countLetters cs cts = foldl (flip addLetter) cts cs

addLetter :: Char -> [(Char, Integer)] -> [(Char, Integer)]
addLetter c [] = [(c, 1)]
addLetter c ((c',cs'):ps) = if c == c' then (c',cs'+1):ps else (c',cs'): addLetter c ps


sortPairs :: (Char, Integer) -> (Char, Integer) -> Ordering
sortPairs (c1, i1) (c2, i2)
    | i1 > i2 = LT
    | i1 < i2 = GT
    | i1 == i2 = compare c1 c2

splitStr :: Char -> String -> [String]
splitStr match c =  case dropWhile (==match) c of
                      "" -> []
                      c' -> w : splitStr match c''
                            where (w, c'') = break (==match) c'

decryptChar :: Int -> Char -> Char
decryptChar id c = chr (((ord c - ord 'a' + id) `mod` 26) + ord 'a')

decryptString :: Int -> String -> String
decryptString id = map (decryptChar id)

decryptRoom :: Int -> [String] -> String -> String
decryptRoom id (cdWd:cdWds) curr = decryptRoom id cdWds (curr ++ decryptString id cdWd ++ " ")
decryptRoom id [] curr = take (length curr - 1) curr

decryptRoomFull :: String -> (String, Int)
decryptRoomFull s =
    let actualCksum = findChecksum s
        (id, assertCksum) = extractIDsum s
    in
        if actualCksum == assertCksum then 
            let 
                roomArray = splitStr '-' s
                actualRoom = take (length roomArray - 1) roomArray
            in 
                (decryptRoom id actualRoom "", id)
        else ("ROOM DOES NOT MATCH", 0)

findRoomMatch :: [(String, Int)] -> String -> Int
findRoomMatch ((code,id):codes) match = 
    if match `isInfixOf` code then id else findRoomMatch codes match
findRoomMatch [] _ = 0
