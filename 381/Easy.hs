-- https://www.reddit.com/r/dailyprogrammer/comments/dv0231/20191111_challenge_381_easy_yahtzee_upper_section/

module Main where

import qualified Data.Map.Strict as M
import           Data.List (foldl')

yahtzee_upper :: [Int] -> Int
yahtzee_upper = maxValue . foldl' (\m x -> M.insertWith (+) x x m) M.empty

maxValue :: (Ord a, Num a) => M.Map k a -> a
maxValue = M.foldl' max 0

testLargeInput :: IO Int
testLargeInput = readFile "inp-easy.txt" >>= return . yahtzee_upper . map read . lines

main :: IO ()
main = testLargeInput >>= putStrLn . show
