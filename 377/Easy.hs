-- https://www.reddit.com/r/dailyprogrammer/comments/bazy5j/20190408_challenge_377_easy_axisaligned_crate/

import Data.List (permutations)

fit1 :: Int -> Int -> Int -> Int -> Int
fit1 x y x' y' = (x `div` x') * (y `div` y')

fit2 :: Int -> Int -> Int -> Int -> Int
fit2 x y x' y' = max (fit1 x y x' y') (fit1 x y y' x')

fit3 :: Int -> Int -> Int -> Int -> Int -> Int -> Int
fit3 x y z x' y' z' = fitn [x, y, z] [x', y', z']

fitn :: [Int] -> [Int] -> Int
fitn ds = maximum . map fitn' . permutations
    where
        fitn' = product . zipWith div ds