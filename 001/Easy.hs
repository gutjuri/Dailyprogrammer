-- https://www.reddit.com/r/dailyprogrammer/comments/pih8x/easy_challenge_1/

main = do
    putStrLn "Name pls"
    name <- getLine
    putStrLn "Age pls"
    age <- getLine
    putStrLn "Username pls"
    uname <- getLine
    putStrLn $ "your name is " ++ name ++ ", you are " ++ age ++ " years old, and your username is " ++ uname