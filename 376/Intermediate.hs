-- https://www.reddit.com/r/dailyprogrammer/comments/b0nuoh/20190313_challenge_376_intermediate_the_revised/

import Control.Applicative

leaps :: Integer -> Integer -> Integer
leaps 0 yr = rule - exception + exexception
    where rule = yr `div` 4
          exception = yr `div` 100
          exexception = 2 * (yr `div` 900)
                        + if yr `mod` 900 >= 200 then 1 else 0
                        + if yr `mod` 900 >= 600 then 1 else 0
leaps a b = leaps 0 (b-1) - leaps 0 (a-1)

isLeap :: Integer -> Bool
isLeap yr = yr `mod` 4 == 0 && (yr `mod` 100 /= 0 || yr `mod` 900 `elem` [200, 600])

tests = pure (uncurry leaps)
        <*> 
        [(2016, 2017), (2019, 2020), (1900, 1901),
        (2000, 2001), (2800, 2801), (123456, 123456),
        (1234, 5678), (123456, 7891011),
        (123456789101112, 1314151617181920)]
