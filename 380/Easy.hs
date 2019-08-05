-- https://www.reddit.com/r/dailyprogrammer/comments/cmd1hb/20190805_challenge_380_easy_smooshed_morse_code_1/

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (foldl', isInfixOf)


-- main challenge
codes :: Map Char String
codes = Map.fromList $ zip ['a'..] $ words ".- -... -.-. -.. . ..-. --. .... .. .--- -.- .-.. -- -. --- .--. --.- .-. ... - ..- ...- .-- -..- -.-- --.."

smorse :: String -> String
smorse = concatMap (codes Map.!)

-- bonus 1
enable1 :: IO [String]
enable1 = readFile "enable1.txt" >>= return . lines

mkMap :: Map String Int -> String -> Map String Int
mkMap s str = Map.insertWith (+) (smorse str) 1 s

countSeq :: [String] -> Map String Int
countSeq = foldl' mkMap Map.empty

-- => "-....-...."
bonus1 :: IO String
bonus1 = do
    ws <- enable1
    return $ head $ Map.keys $ Map.filter (==13) $ countSeq ws
    
-- bonus 2
has15Dashes :: String -> Bool
has15Dashes = isInfixOf (replicate 15 '-')

-- => "bottommost"
bonus2 :: IO String
bonus2 = enable1 >>= return . head . filter (has15Dashes . smorse)

-- bonus 3
isPerfectlyBalanced :: String -> Bool
isPerfectlyBalanced = (==0) . foldl (\n c -> if c == '-' then n+1 else n-1) 0

-- => ["counterdemonstrations","overcommercialization"]
bonus3 :: IO [String]
bonus3 = enable1 >>= return  . filter (isPerfectlyBalanced . smorse) . filter ((==21) . length)

-- bonus 4
isPalindrome :: String -> Bool
isPalindrome s = s == reverse s

-- => "intransigence"
bonus4 :: IO String
bonus4 = enable1 >>= return . head . filter (isPalindrome . smorse) . filter ((==13) . length)

-- bonus 5
seqs :: Set String
seqs = Set.fromList $ f 13
    where f 0 = [""]
          f n = ".-" >>= \c -> f (n-1) >>= \xs -> return (c:xs)

sublists13 :: String -> [String]
sublists13 (x1:(xs@(x2:x3:x4:x5:x6:x7:x8:x9:x10:x11:x12:x13:_))) = [x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13] : sublists13 xs
sublists13 _ = []

-- => ["---.----.----","---.---.-----","---.---.---.-","--.---.------","--.---.---.--"]
bonus5 :: IO [String]
bonus5 = enable1 >>= return . Set.toList . foldl' (flip Set.delete) seqs . concatMap (sublists13 . smorse)
