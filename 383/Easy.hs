{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Data.Maybe
import           Data.List
import qualified Data.Map.Strict               as M
import Data.Ord (comparing)
import Data.Function (on)


same_necklace :: Text -> Text -> Bool
same_necklace a b = lowest_rep a == lowest_rep b

lowest_rep :: Text -> Text
lowest_rep str = minimum $ rotations str

rotations :: Text -> [Text]
rotations str = rotations' (T.length str - 1) str
 where
  rotations' :: Int -> Text -> [Text]
  rotations' _ ""  = [""]
  rotations' 0 str = [nextrot str]
  rotations' n str = let next = nextrot str in next : rotations' (n - 1) next
  nextrot str = (T.last str) `T.cons` (T.init str)

repeats :: Text -> Int
repeats str = length $ elemIndices (minimum rts) rts where rts = rotations str

bonus2 :: [Text] -> [Text]
bonus2 = map fst . fromJust . find ((==4) . length) . groupBy ((==) `on` snd) . sortBy (comparing snd) . map (\str -> (str, lowest_rep str))

main :: IO ()
main = do
  print $ same_necklace "nicole" "icolen"
  print $ same_necklace "nicole" "lenico"
  print $ same_necklace "nicole" "coneli"
  print $ same_necklace "aabaaaaabaab" "aabaabaabaaa"
  print $ same_necklace "abc" "cba"
  print $ same_necklace "xxyyy" "xxxyy"
  print $ same_necklace "xyxxz" "xxyxz"
  print $ same_necklace "x" "x"
  print $ same_necklace "x" "xx"
  print $ same_necklace "x" ""
  print $ same_necklace "" ""
  print $ repeats "abc"
  print $ repeats "abcabcabc"
  print $ repeats "abcabcabcx"
  print $ repeats "aaaaaa"
  print $ repeats "a"
  print $ repeats ""
  TIO.readFile "enable1.txt" >>= (print . bonus2 . T.lines)
