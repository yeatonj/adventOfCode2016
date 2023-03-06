import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy.Char8 as C
import System.IO
import Debug.Trace
import Data.Char(digitToInt)

main = do
    handle <- openFile "data.txt" ReadMode
    contents <- hGetContents handle
    -- let res = findPW contents 1 ""
    let res = findPWpt2 contents 1 "        "
    print res
    hClose handle

findPW :: String -> Int -> String -> String
findPW strIn curInd curPW =
    if length curPW == 8
        then curPW
        else findPW strIn (curInd + 1) (curPW ++ updatePW strIn curInd)

updatePW :: String -> Int -> String
updatePW strIn curInd =
    let curStr = strIn ++ show curInd
        hsh = show (md5 (C.pack curStr))
    in
        if take 5 hsh == "00000"
            then [hsh!!5]
            else ""

findPWpt2 :: String -> Int -> String -> String
findPWpt2 strIn curInd curPW =
    if ' ' `elem` curPW
        then findPWpt2 strIn (curInd + 1) (updatePWpt2 strIn curInd curPW)
        else curPW

updatePWpt2 :: String -> Int -> String -> String
updatePWpt2 strIn curInd curPW = 
    let curStr = strIn ++ show curInd
        hsh = show (md5 (C.pack curStr))
    in
        if take 5 hsh == "00000"
            then addCharToPW curPW (hsh!!6) (digitToInt (hsh!!5))
            else curPW

addCharToPW :: String -> Char -> Int -> String
addCharToPW (firstChPW:restPW) newCh updIndex
    | updIndex > 7 || (updIndex == 0 && firstChPW /= ' ') = firstChPW:restPW
    | updIndex == 0 = newCh:restPW
    | otherwise = firstChPW:addCharToPW restPW newCh (updIndex - 1)