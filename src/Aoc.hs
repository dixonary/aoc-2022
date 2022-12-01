module Aoc where

import Witch ( From(..) )
import Data.List.Split ( splitOn )
import Data.List

--------------------------------------------------------------------------------
-- DAY 01

newtype Day01 = Day01 [Integer]
instance From String Day01 where 
  from = Day01 . reverse . sort 
       . map (sum . map read . splitOn "\n") 
       . splitOn "\n\n"

day01a :: Day01 -> Integer
day01a (Day01 cs) = head cs

day01b :: Day01 -> Integer
day01b (Day01 cs) = sum $ take 3 cs