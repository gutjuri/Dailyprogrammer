-- https://www.reddit.com/r/dailyprogrammer/comments/afxxca/20190114_challenge_372_easy_perfectly_balanced/

import Data.List

balanced :: String -> Bool
balanced = balancedBonus

-- bonus

balancedBonus :: String -> Bool
balancedBonus = (<=1) . length .  group . map length . group . sort