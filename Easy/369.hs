-- https://www.reddit.com/r/dailyprogrammer/comments/a0lhxx/20181126_challenge_369_easy_hex_colors/

import Text.Printf
import Data.List
import Data.Maybe
import Data.Char

-- Main challenge

hexcolor :: Int -> Int -> Int -> String
hexcolor r g b = concat ["#", toHexStr r, toHexStr g, toHexStr b]
    where
        toHexStr = printf "%02x"

-- Bonus
        
hexToInt :: String -> Int
hexToInt hex = foldl f 0 (map toLower hex)
    where
        f      a c = 16 * a + hexChar c
        hexChar  c = fromMaybe (error "Illegal Color String") (elemIndex c hexSymbols)
        hexSymbols = "0123456789abcdef"

blend :: [String] -> String
blend hexs = tupleToHexcolor (divRgb (foldl sumRgb (0, 0, 0) (map toIntTuple hexs)) (length hexs))
    where
        toIntTuple h                        = (colorVal 1 h, colorVal 3 h, colorVal 5 h)
        sumRgb (rv, gv, bv) (rv', gv', bv') = (rv+rv', gv+gv', bv+bv')
        divRgb (rv, gv, bv) n               = (div rv n, div gv n, div bv n)
        colorVal x h                        = hexToInt $ drop x $ take (x+2) h
        tupleToHexcolor (r, b, g)           = hexcolor r b g
        