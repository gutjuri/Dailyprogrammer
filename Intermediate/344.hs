-- https://www.reddit.com/r/dailyprogrammer/comments/7jkfu5/20171213_challenge_344_intermediate_bankers/

import Control.Monad.State 
import Data.List 
import Data.Maybe

-- parsing stuff and main
type Ressources = (Int, Int, Int)

parseLine :: String -> (Ressources, Ressources)
parseLine ln = let nums = map read $ words ln in (toTuple (take 3 nums), toTuple (drop 3 nums))

toTuple :: [Int] -> Ressources
toTuple (a:b:c:_) = (a, b, c)

runWithInputFile :: String -> IO ()
runWithInputFile path = do
    -- read and parse input file
    input <- readFile path
    let (strInRc:strReqRcs) = lines input
    let initialState = toTuple $ map read $ words strInRc
    let prcs = map (\(n, (x, y)) -> (n, x, y)) $ zip [0..] $ map parseLine strReqRcs
    
    -- run banker's algorithm and print result
    let result = case evalState (banker prcs) initialState of
                    Nothing -> "No result"
                    Just x  -> concat $ map (\n -> "p" ++ show n ++ " ") x
    putStrLn result


-- Correct output is:
-- p1 p3 p0 p2 p4 (or some other correct solution)
-- No result
main :: IO ()
main = do
    runWithInputFile "input/344.txt"
    runWithInputFile "input/344f.txt"

-- interesting stuff 
   


banker :: [(Int, Ressources, Ressources)] -> State Ressources (Maybe [Int])
banker [] = return $ Just []
banker xs = do 
                st <- get
                let mNextP :: Maybe (Int, Ressources, Ressources)
                    mNextP = find (canRun st) xs
                case mNextP of
                    Nothing         -> return Nothing
                    Just x@(p, r, _)->  do  put (st `addRes` r)
                                            let nextXs = delete x xs
                                            runAfter <- banker nextXs
                                            case runAfter of
                                                Nothing -> return Nothing
                                                Just ls -> return $ Just (p:ls)
            where
                canRun s1 (_, cR, maxR) = s1 `addRes` cR >= maxR
                addRes (s1, s2, s3) (r1, r2, r3) = (s1+r1, s2+r2, s3+r3)
