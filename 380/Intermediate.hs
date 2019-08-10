-- https://www.reddit.com/r/dailyprogrammer/comments/cn6gz5/20190807_challenge_380_intermediate_smooshed/

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad (forM)
import Data.Maybe (fromJust, catMaybes, listToMaybe)

-- Main Challenge

codes :: Map String Char
codes = Map.fromList 
    $ zip (words ".- -... -.-. -.. . ..-. --. .... .. .--- -.- .-.. -- -. --- .--. --.- .-. ... - ..- ...- .-- -..- -.-- --..")
    ['a'..]

smalpha :: String -> String
smalpha = reverse . fromJust . smalpha' codes ""
    where 
        smalpha' :: Map String Char -> String -> String -> Maybe String
        smalpha' _ acc ""  = Just acc
        smalpha' m acc enc = listToMaybe $ catMaybes $  map (nextItr m acc enc) [4, 3, 2, 1]
        nextItr :: Map String Char -> String -> String -> Int -> Maybe String
        nextItr m acc enc n = do
            let nextchr = take n enc
            let rest    = drop n enc
            chr <- Map.lookup nextchr m
            smalpha' (Map.delete nextchr m) (chr:acc) rest

-- Bonus 1

bonus1 :: IO ()
bonus1 = readFile "inputIntermediate.txt" >>= sequence_ . map putStrLn . map smalpha . lines
