{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Data.List
import qualified Data.Map.Strict               as M

sameNecklace :: Text -> Text -> Bool
sameNecklace a b = lowestRep a == lowestRep b

lowestRep :: Text -> Text
lowestRep str = minimum $ rotations str

rotations :: Text -> [Text]
rotations str = rotations' (T.length str - 1) str
 where
  rotations' :: Int -> Text -> [Text]
  rotations' _ ""  = [""]
  rotations' 0 str = [nextrot str]
  rotations' n str = let next = nextrot str in next : rotations' (n - 1) next
  nextrot str = T.last str `T.cons` T.init str

repeats :: Text -> Int
repeats str = length $ elemIndices (minimum rts) rts where rts = rotations str

bonus2 :: [Text] -> [Text]
bonus2 enable1 =
  let m4 = min4occ enable1 in filter ((== m4) . lowest_rep) enable1
 where
  min4occ = head . M.keys . M.filter (== 4) . M.fromListWith (+) . map
    (\str -> (lowestRep str, 1))
