-- https://www.reddit.com/r/dailyprogrammer/comments/99d24u/20180822_challenge_366_intermediate_word_funnel_2/

import           Data.List
import           Data.Maybe
import           Data.Function
import qualified Data.Set as Set

-- We use functions from the easy part of the challenge
import           Easy (funnel, funnels, allRem1, enable1)

-- Main challenge
-- Without longest funnel
funnel2 :: Set.Set String -> String -> Int
funnel2 wl = (+1) . maximum . (0:) . map (funnel2 wl) . funnels wl

-- With longest funnel
funnel2' :: Set.Set String -> String -> (Int, [String])
funnel2' wl str = (\(n, l) -> (n+1, str:l)) $ maximum $ ((0, []):) $ map (funnel2' wl) $ funnels wl str

performFunnel2 :: String -> IO Int
performFunnel2 str = do
    words <- enable1
    return $ funnel2 words str
    
performFunnel2' :: String -> IO (Int, [String])
performFunnel2' str = do
    words <- enable1
    return $ funnel2' words str

-- Bonus
funnel10 :: Set.Set String -> String
funnel10 wl = fromJust $ find (\w -> funnel2 wl w == 10) $ reverse $ sortBy (compare `on` length) $ Set.toList wl

performFunnel10 :: IO String
performFunnel10 = do
    words <- enable1
    return $ funnel10 words
    