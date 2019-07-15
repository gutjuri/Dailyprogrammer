--https://www.reddit.com/r/dailyprogrammer/comments/cdieag/20190715_challenge_379_easy_progressive_taxation/

    import Data.Bifunctor

    type TaxTable = [(Integer, Double)]

    -- challenge

    tax' :: TaxTable -> Integer -> Integer
    tax' [] _                          = 0
    tax' ((threshold, rate):xs) income = inBracket + rest
        where inBracket = floor $ (fromIntegral (min income threshold)) * rate
              rest      = tax' (map (\(ts, r) -> (ts - threshold, r)) xs) (max 0 (income - threshold))

    -- bonus 1

    readTable :: FilePath -> IO TaxTable
    readTable path = do
        ratesAsStrs <- map words <$> lines <$> readFile path
        return $ map (bimap read read) $ map (\[k, v] -> (k, v)) ratesAsStrs
        
    tax :: Integer -> IO Integer
    tax income = readTable "input/taxtable.txt" >>= return . flip tax' income

    -- bonus 2

    binSearch :: TaxTable -> Double -> Integer -> Integer-> Integer
    binSearch table rate lower upper 
        | abs (rate' - rate) < 0.001 = mid
        | mid == lower               = -1
        | rate' > rate               = binSearch table rate lower (mid-1)
        | otherwise                  = binSearch table rate (mid+1) upper
        where
            mid = (upper + lower) `div` 2
            rate' = fromIntegral (tax' table mid)  / fromIntegral mid 

    overall :: Double -> IO Integer
    overall rate = readTable "input/taxtable.txt" >>= return . (\table -> binSearch table rate 0 (maxInc table))
        where maxInc = sum . map fst
        
    runTests :: IO [Integer]
    runTests = mapM overall [0.00, 0.06, 0.09, 0.32, 0.40]
