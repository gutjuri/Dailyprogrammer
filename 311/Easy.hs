import Data.Bits

isJolly :: [Int] -> Bool
isJolly (n:xs) = isJolly' zeroBits xs
    where
        isJolly' :: Integer -> [Int] -> Bool
        isJolly' b [_] = popCount b == n-1
        isJolly' b (x:x':xs') = isJolly' (b `setBit` (abs (x-x'))) (x':xs')
        
test =      isJolly [4, 1, 4, 2, 3]
    && not (isJolly [5, 1, 4, 2, -1, 6])
    && not (isJolly [4, 19, 22, 24, 21])
    &&      isJolly [4, 19, 22, 24, 25]
    &&      isJolly [4, 2, -1, 0, 2]