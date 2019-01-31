-- https://www.reddit.com/r/dailyprogrammer/comments/akv6z4/20190128_challenge_374_easy_additive_persistence/

import Data.List
import Data.Maybe

digitSum :: Integer -> Integer
digitSum 0 = 0
digitSum n = n `mod` 10 + digitSum (n `div` 10)

-- naive
additivePersistence :: Integer -> Int
additivePersistence n | n < 10 = 0
                      | otherwise = 1 + additivePersistence (digitSum n)

-- still naive but slightly more interesting
additivePersistence' :: Integer -> Int
additivePersistence' n = fromJust $ findIndex (<10) digitSums
    where digitSums = n : map digitSum digitSums