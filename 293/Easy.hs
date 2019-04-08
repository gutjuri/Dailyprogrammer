-- https://www.reddit.com/r/dailyprogrammer/comments/5e4mde/20161121_challenge_293_easy_defusing_the_bomb/

import Data.Map.Strict (Map, fromList, (!))

rules :: Map String [String]
rules = fromList [
            ("white", ["red", "orange", "green", "purple"]),
            ("red", ["green"]),
            ("black",  ["red", "black", "purple"]),
            ("orange", ["red", "black"]),
            ("green", ["orange", "white"]),
            ("purple", ["red", "black"])
        ]

checkSequence :: [String] -> Bool
checkSequence [x] = True
checkSequence (x:x':xs) = x' `elem` (rules ! x) && checkSequence (x':xs)

main :: IO ()
main = do
    input <- getLines
    let success = checkSequence input
    if success then
        putStrLn "Bomb defused"
    else
        putStrLn "Boom"

getLines :: IO [String]        
getLines = do
    g <- getLine
    if g == "" then
        return []
    else do
        rest <- getLines
        return $ g:rest
        