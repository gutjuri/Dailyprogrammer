-- https://www.reddit.com/r/dailyprogrammer/comments/9cvo0f/20180904_challenge_367_easy_subfactorials_another/
-- Finds the number of derangements of [n]

--subfac :: Integral a => a -> Integer
subfac n = round (toRational $ product [1..n] / e)
    where e = exp 1