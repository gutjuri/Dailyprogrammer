-- https://www.reddit.com/r/dailyprogrammer/comments/a72sdj/20181217_challenge_370_easy_upc_check_digits/

upca :: Integer -> Integer
upca = (`mod` 10) . (flip subtract 10) . (`mod` 10) . sum . zipWith (*) (cycle [3, 1]) . digits 11

digits :: Integral a => Int -> a -> [a]
digits n k = [ k `mod` 10^(x+1) `div` 10^x | x <- [(n-1), (n-2)..0] ]