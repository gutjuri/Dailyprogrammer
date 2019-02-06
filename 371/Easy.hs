-- https://www.reddit.com/r/dailyprogrammer/comments/ab9mn7/20181231_challenge_371_easy_n_queens_validator/

import Control.Applicative
import Data.List
import Data.Maybe

qcheck :: [Int] -> Bool
qcheck = not . or . (<*>) [hasDups, hasOnSameDiag, hasOnSameDiag . reverse] . wrap

wrap :: [a] -> [[a]]
wrap = (:[])

hasDups :: [Int] -> Bool
hasDups = hasDups' . sort
    where hasDups' = fst . foldl (\(b, x') x -> (b || x == x', x)) (False, 0)

hasOnSameDiag :: [Int] -> Bool
hasOnSameDiag = hasDups . zipWith (+) [1..]

testQcheck :: Bool
testQcheck = qcheck [4, 2, 7, 3, 6, 8, 5, 1]
          && qcheck [2, 5, 7, 4, 1, 8, 6, 3]
          && not (qcheck [5, 3, 1, 4, 2, 8, 6, 3])
          && not (qcheck [5, 8, 2, 4, 7, 1, 3, 6])
          && not (qcheck [4, 3, 1, 8, 1, 3, 5, 2])

-- Bonus

qfix :: [Int] -> Maybe [Int]
qfix xs = find qcheck $ map (\(i, j) -> swap i j xs) [(x, y) | x <- [0..len], y <- [1..len], x /= y]
    where len = length xs - 1

swap :: Int -> Int -> [a] -> [a]
swap i j xs | i > j = swap j i xs
swap i j xs = (take i xs) ++ [elemB] ++ (take (j-i-1) (drop (i+1) xs)) ++ [elemA] ++ drop (j+1) xs
    where elemA = xs !! i
          elemB = xs !! j
          
testQfix :: Bool
testQfix = (fromMaybe [] (qfix [8, 6, 4, 2, 7, 1, 3, 5])) == [4, 6, 8, 2, 7, 1, 3, 5]
        && (fromMaybe [] (qfix [8, 5, 1, 3, 6, 2, 7, 4])) == [8, 4, 1, 3, 6, 2, 7, 5]
        && (fromMaybe [] (qfix [4, 6, 8, 3, 1, 2, 5, 7])) == [4, 6, 8, 3, 1, 7, 5, 2]
        && (fromMaybe [] (qfix [7, 1, 3, 6, 8, 5, 2, 4])) == [7, 3, 1, 6, 8, 5, 2, 4]