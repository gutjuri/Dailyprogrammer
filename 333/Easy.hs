-- https://www.reddit.com/r/dailyprogrammer/comments/72ivih/20170926_challenge_333_easy_packet_assembler/

import Data.List (span, sort)
import Data.Char (isDigit, isSpace)
import qualified Data.Map as Map

type Packet = (Int, Int, Int, String)

readPackage :: String -> Packet
readPackage pkg = let (mnum, r) = consumeInt pkg
                      (pnum, r') = consumeInt r
                      (tpkg, msg) = consumeInt r'
                  in (mnum, pnum, tpkg, msg)
    where
        consumeInt :: String -> (Int, String)
        consumeInt xs = let (i, r) = span isDigit xs in (read i, dropWhile isSpace r)

showMsg :: [Packet] -> String
showMsg = concatMap (\(_, _, _, m) -> m) . sort
        
runPkgSorter :: Map.Map Int [Packet] -> IO ()
runPkgSorter map = do
    inp <- getLine
    if inp == "" then
        return ()
    else do
        let pkg@(nm, _, np, _) = readPackage inp
            map' = Map.insertWith (++) nm [pkg] map
            pkgs = map' Map.! nm
        if length pkgs == np then do
            putStrLn $ showMsg pkgs
            runPkgSorter (Map.delete nm map')
        else
            runPkgSorter map'
        
main :: IO ()
main = do
    runPkgSorter Map.empty