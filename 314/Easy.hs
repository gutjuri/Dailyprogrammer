-- https://www.reddit.com/r/dailyprogrammer/comments/69y21t/20170508_challenge_314_easy_concatenated_integers/

import Data.List (sortBy)

challenge = do
    inp <- getLine
    let nums = map read $ words inp
    let smallest = sortBy comp nums
    let largest = reverse smallest
    putStr $ concatMap show $ smallest
    putStr " "
    putStrLn $ concatMap show $ largest


comp :: Int -> Int -> Ordering
comp a b = compare (a' ++ b') (b' ++ a')
    where (a', b') = (show a, show b)
