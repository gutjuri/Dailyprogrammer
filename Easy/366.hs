-- https://www.reddit.com/r/dailyprogrammer/comments/98ufvz/20180820_challenge_366_easy_word_funnel_1/

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Simple
import           System.Directory
import           Data.List
import           Data.Function
import qualified Data.Set as Set

-- main challenge

funnel :: String -> String -> Bool
funnel l s = elem s $ allRem1 l
 
allRem1 :: String -> [String]
allRem1 [] = []
allRem1 (x:xs) = xs : map (x:) (allRem1 xs)

-- bonus 1

enable1 :: IO (Set.Set String)
enable1 = do
    let tmpfile = "enable1.txt.tmp"
    cached <- doesFileExist tmpfile
    if not cached then do
        response <- httpLBS "https://raw.githubusercontent.com/dolph/dictionary/master/enable1.txt"
        let str = L8.unpack $ getResponseBody response
        writeFile tmpfile str
        return $ Set.fromList $ lines str
    else do
        words <- readFile tmpfile
        return $ Set.fromList $ lines words
    

bonus :: String -> IO [String]
bonus str = do
    words <- enable1
    return $ map head $ group $ sort $ filter (flip Set.member words) $ allRem1 str
    
-- bonus 2

bonus2 :: IO [String]
bonus2 = do
    wSet <- enable1
    let funnel5List = Set.toList $ Set.filter (\w -> funnelNum w wSet == 5) wSet
    putStrLn $ "Number of matching words: " ++ (show $ length funnel5List)
    return funnel5List

funnelNum :: String -> Set.Set String -> Int
funnelNum str = Set.size . Set.intersection (Set.fromList $ allRem1 str)
