{-# LANGUAGE DataKinds #-}
module Aoc where

import Witch ( From(..) )
import Data.List.Split ( splitOn )
import Data.List

import Data.Finite

--------------------------------------------------------------------------------
-- DAY 1

parse01 :: String -> [Integer]
parse01 = reverse . sort . map (sum . map read . lines) . splitOn "\n\n"

day01a :: [Integer] -> Integer
day01a = head

day01b :: [Integer] -> Integer
day01b = sum . take 3

--------------------------------------------------------------------------------
-- DAY 2

type RPS = Finite 3
instance {-# OVERLAPS #-} Read RPS where 
  readsPrec _ (c:t) = singleton $ (,t) $ case c of
    'A' -> 0; 'B' -> 1; 'C' -> 2
    'X' -> 0; 'Y' -> 1; 'Z' -> 2

parse02 :: String -> [(RPS,RPS)]
parse02 = map ((\[x,y] -> (x,y)) . map read . words). lines

day02a :: [(RPS,RPS)] -> Integer
day02a = sum . map (\(x,y) -> getFinite y + 1 + getFinite (y-x+1) * 3)

day02b :: [(RPS,RPS)] -> Integer
day02b = sum . map (\(x,y) -> getFinite (x+y+2) + 1 + getFinite y * 3)