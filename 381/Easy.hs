-- https://www.reddit.com/r/dailyprogrammer/comments/dv0231/20191111_challenge_381_easy_yahtzee_upper_section/

module Main where

import qualified Data.HashMap.Strict as M
import           Data.List (foldl')

yahtzee_upper :: [Int] -> Int
yahtzee_upper = maxValue . foldl' (\m x -> M.insertWith (+) x x m) M.empty

maxValue :: Ord a => M.HashMap k a -> a
maxValue = maximum . map snd . M.toList

testLargeInput :: IO Int
testLargeInput = yahtzee_upper . map read . lines <$> readFile "inp-easy.txt" 

main :: IO ()
main = print =<< testLargeInput