-- https://www.reddit.com/r/dailyprogrammer/comments/bqy1cf/20190520_challenge_378_easy_the_havelhakimi/

    import Data.List (sortBy)

    warmup1 :: [Int] -> [Int]
    warmup1 = filter (/=0)

    warmup2 :: [Int] -> [Int]
    warmup2 = sortBy (flip compare)

    warmup3 :: Int -> [Int] -> Bool
    warmup3 n xs = n > length xs

    warmup4 :: Int -> [Int] -> [Int]
    warmup4 n xs = (map (subtract 1) $ take n xs) ++ drop n xs

    hh :: [Int] -> Bool
    hh aws = case warmup2 $ warmup1 aws of
                [] -> True
                (x:xs) -> if warmup3 x xs then False else hh $ warmup4 x xs

--tests :: Bool
tests = not (any hh 
                [[5, 3, 0, 2, 6, 2, 0, 7, 2, 5],
                [4, 2, 0, 1, 5, 0],
                [15, 18, 6, 13, 12, 4, 4, 14, 1, 6, 18, 2, 6, 16, 0, 9, 10, 7, 12, 3],
                [6, 0, 10, 10, 10, 5, 8, 3, 0, 14, 16, 2, 13, 1, 2, 13, 6, 15, 5, 1],
                [2, 2, 0],
                [3, 2, 1],
                [1]]
            )
        &&
        all hh 
            [[3, 1, 2, 3, 1, 0],
            [16, 9, 9, 15, 9, 7, 9, 11, 17, 11, 4, 9, 12, 14, 14, 12, 17, 0, 3, 16],
            [14, 10, 17, 13, 4, 8, 6, 7, 13, 13, 17, 18, 8, 17, 2, 14, 6, 4, 7, 12],
            [1, 1],
            []]
            
             