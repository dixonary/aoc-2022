module Aoc where

import Witch ( From(..) )
import Data.List.Split ( splitOn )
import Data.List

--------------------------------------------------------------------------------
-- DAY 01

parse01 :: String -> [Integer]
parse01 = reverse . sort . map (sum . map read . lines) . splitOn "\n\n"

day01a :: [Integer] -> Integer
day01a = head

day01b :: [Integer] -> Integer
day01b = sum . take 3