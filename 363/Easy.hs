-- https://www.reddit.com/r/dailyprogrammer/comments/8q96da/20180611_challenge_363_easy_i_before_e_except/

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Simple
import           System.Directory
import           Control.Applicative
import Data.List

-- main challenge

check :: String -> Bool
check = check' 'c'

check' :: Char -> String -> Bool
check' c str = not (isInfixOf (c:"ie") str) &&
            (not (isInfixOf "ei" str) || isInfixOf (c:"ei") str)

-- bonus 1

bonus1 :: IO Int
bonus1 = countExceptions 'c'

-- bonus 2

bonus2 :: IO ()
bonus2 = do
    exceptions <- mapM (\c -> countExceptions c) ['a'..'z']
    let strings = zipWith (\c v -> "There are " ++ show v ++ " exceptions for " ++ show c) ['a'..] exceptions
    sequence_ $ map putStrLn strings

countExceptions :: Char -> IO Int
countExceptions c = do
    words <- enable1
    return $ length $ filter (not . check' c) words
    
-- enable 1

enable1 :: IO [String]
enable1 = do
    let tmpfile = "enable1.txt.tmp"
    cached <- doesFileExist tmpfile
    if not cached then do
        response <- httpLBS "https://raw.githubusercontent.com/dolph/dictionary/master/enable1.txt"
        let str = L8.unpack $ getResponseBody response
        writeFile tmpfile str
        return $ lines str
    else do
        words <- readFile tmpfile
        return $ lines words