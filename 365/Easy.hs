-- https://www.reddit.com/r/dailyprogrammer/comments/8xbxi9/20180709_challenge_365_easy_uparrow_notation/

up :: Integer -> Integer -> Integer -> Integer
up 1 x y = x^y
up _ _ 0 = 1
up n x y = up (n-1) x (up n x (y-1))