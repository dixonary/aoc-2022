{-# LANGUAGE DataKinds #-}
module Aoc where

import Witch ( From(..) )

import Data.Char (ord)


import Data.Finite ( getFinite, Finite )

import Data.List.Split ( splitOn, chunksOf )
import Data.List ( intersect, singleton, sort )

import Data.Set qualified as Set
import Data.Set (Set)

import Data.Text qualified as Text
import Data.Text (Text)

import Data.Attoparsec.Text hiding (take)
import Control.Applicative.Combinators ()
import Utils.Parsers

import Data.Function (on)


--------------------------------------------------------------------------------
-- Utilibobs

both :: (a -> b) -> (a,a) -> (b,b)
both f (x,y) = (f x, f y)


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
day02b = sum . map (\(x,y) -> getFinite (x+y-1) + 1 + getFinite y * 3)


--------------------------------------------------------------------------------
-- DAY 3

type Rucksack = (Set Char, Set Char)
instance {-# OVERLAPS #-} Read Rucksack where
  readsPrec _ str = singleton 
    $ (,[]) 
    $ both Set.fromList 
    $ splitAt (length str `div` 2) str

parse03 :: String -> [Rucksack]
parse03 = map read . lines

priority :: Char -> Integer
priority c
  | c <= 'Z'  = from $ ord c - ord 'A' + 27
  | otherwise = from $ ord c - ord 'a' + 1

day03a :: [Rucksack] -> Integer
day03a = sum . map (priority . Set.findMin . uncurry Set.intersection) 

day03b :: [Rucksack] -> Integer
day03b = sum . map (priority . Set.findMin . foldr1 Set.intersection) 
       . chunksOf 3 . map (uncurry (<>))


--------------------------------------------------------------------------------
-- DAY 4

parse04 :: String -> [((Integer,Integer),(Integer,Integer))]
parse04 = either error id . parseOnly p . from
  where p = decimal `around` char '-' `around` char ',' `sepBy` endOfLine

day04a :: [((Integer,Integer),(Integer,Integer))] -> Int
day04a = length . filter 
  (\((a,b),(x,y)) -> (a>=x&&b<=y) || (x>=a&&y<=b))

day04b :: [((Integer,Integer),(Integer,Integer))] -> Int
day04b = length . filter 
  (\((a,b),(x,y)) -> (a>=x&&a<=y) || (b>=x&&b<=y) || (a <= x && b >= y))