{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}
module Aoc where

import Prelude hiding (takeWhile)

import Witch ( From(..) )

import Data.Char (ord)


import Data.Finite ( getFinite, Finite )

import Data.List.Split ( splitOn, chunksOf )
import Data.List ( intersect, singleton, sort, transpose, findIndex, nub )
import Data.List qualified as List 

import Data.Set qualified as Set
import Data.Set (Set)

import Data.Map qualified as Map
import Data.Map (Map)

import Data.Text qualified as Text
import Data.Text (Text)

import Data.Attoparsec.Text hiding (take, sepBy, sepBy1, count)
import Control.Applicative.Combinators
    ( (<|>), count, sepBy, sepBy1 )
import Data.Char (isUpper)
import Utils.Parsers as P
import Utils.Utils as U

import Data.Ord
import Data.Bifunctor
import Data.Functor
import Data.Foldable
import Data.Function 
import Control.Applicative

import Data.Maybe
import Data.Bool

import System.FilePath

import Debug.Trace

import Text.Pretty.Simple

--------------------------------------------------------------------------------
-- Utilibobs

both :: (a -> b) -> (a,a) -> (b,b)
both f (x,y) = (f x, f y)

takeToEnd :: From Text a => Parser a
takeToEnd = (from <$> takeTill (=='\n')) <* (void (char '\n') <|> endOfInput)

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

parse04 :: Parser [((Integer,Integer),(Integer,Integer))]
parse04 = decimal `around` char '-' `around` char ',' `sepBy` endOfLine

day04a :: [((Integer,Integer),(Integer,Integer))] -> Int
day04a = length . filter 
  (\((a,b),(x,y)) -> (a>=x&&b<=y) || (x>=a&&y<=b))

day04b :: [((Integer,Integer),(Integer,Integer))] -> Int
day04b = length . filter 
  (\((a,b),(x,y)) -> (a>=x&&a<=y) || (b>=x&&b<=y) || (a<=x&&b>=y))

--------------------------------------------------------------------------------
-- DAY 5

type Day05 = (Map Int [Char], [(Int,Int,Int)])
parse05 :: Parser Day05
parse05 = let
  crate = ("[" *> letter <* "]" <&> Just) <|> (count 3 space $> Nothing)
  line  = liftA3 (,,) ("move "*>decimal) (" from "*>decimal) (" to "*>decimal)

  in do
  crates <- Map.fromList . zip [1..] . map catMaybes . transpose
              <$> crate `sepBy` " " `sepBy` endOfLine
  takeTill (== 'm')
  lines <- line `sepBy1` endOfLine
  pure (crates, lines)

doMoves f = foldl' \piles (n,x,y) ->
  let p = f $ take n $ piles Map.! x
  in piles & Map.adjust (drop n) x & Map.adjust (p ++) y

day05a :: Day05 -> String
day05a = map head . Map.elems . uncurry (doMoves reverse)

day05b :: Day05 -> String
day05b = map head . Map.elems . uncurry (doMoves id)

--------------------------------------------------------------------------------
-- DAY 6

parse06 :: String -> Text
parse06 = from

findSignal :: Int -> Text -> Int
findSignal k = fromJust . findIndex ((==k) . Set.size) 
             . map (Set.fromList . Text.unpack . Text.takeEnd k) 
             . Text.inits

day06a :: Text -> Int
day06a = findSignal 4

day06b :: Text -> Int
day06b = findSignal 14

--------------------------------------------------------------------------------
-- DAY 7

data FSNode = File Integer | Dir [FilePath] deriving (Show)
type FS = Map FilePath FSNode

addEntry :: FilePath -> FSNode -> FSNode
addEntry s (Dir ns) = Dir (nub $ s:ns)

isDir :: FSNode -> Bool
isDir = \case { Dir _ -> True; _ -> False }

getSize :: FS -> FSNode -> Integer
getSize fs (File n) = n
getSize fs (Dir ns) = sum $ map (getSize fs . (fs Map.!)) ns


parse07 :: Parser FS
parse07 = do
  takeToEnd @Text

  let 
    addChild :: String -> FS -> (FilePath,FSNode) -> FS
    addChild fp fs (n,node) = fs 
      & Map.insert (fp </> n) node
      & Map.adjust (addEntry (fp </> n)) fp

    parseCommand :: FilePath -> FS -> Parser FS
    parseCommand currentNode fs = goUp <|> enterDir <|> ls <|> done
      where
        done = endOfInput $> fs
        goUp = "$ cd ..\n" *> parseCommand (takeDirectory currentNode) fs
        enterDir = do
          "$ cd "
          name <- takeToEnd
          parseCommand (currentNode </> name) fs
        ls = do
          "$ ls\n"
          nodes <- many $
                 (do "dir "           ; n <- takeToEnd; pure $ (n,Dir []))
             <|> (do s <- decimal; " "; n <- takeToEnd; pure $ (n,File s))
          parseCommand currentNode $ foldl' (addChild currentNode) fs nodes

  parseCommand "/" $ Map.singleton "/" (Dir [])

day07a :: FS -> Integer
day07a fs = sum $ Map.filter (<=100000) 
          $ Map.map (getSize fs) $ Map.filter isDir fs

day07b :: FS -> Integer
day07b fs = let
  usedSpace = getSize fs $ fs Map.! "/"
  freeSpace = 70000000 - usedSpace
  needToFree = 30000000 - freeSpace

  in minimum 
    $ Map.elems $ Map.filter (>= needToFree) 
    $ Map.map (getSize fs) $ Map.filter isDir fs

--------------------------------------------------------------------------------
-- DAY 8

parse08 = coordinateParser (Just . read @Integer . List.singleton) 0

day08a :: Map (Int, Int) Integer -> Int
day08a m = let
  (l,r,t,b) = mapBoundingBox m

  passesX   = [[(x,y) | x <- l `to` r] | y <- t `to` b]
  passesY   = [[(x,y) | y <- t `to` b] | x <- l `to` r]
  allPasses = [passesX, map reverse passesX, passesY, map reverse passesY]

  markVis (m,k) p = let (h,_) = m Map.! p in 
    if h > k then (Map.adjust (second (const True)) p m, h) 
    else          (m,k)

  in m
    & Map.map (,False)
    & foldl' (foldl' (\m -> fst . foldl' markVis (m,-1))) `flip` allPasses
    & Map.elems & filter snd & length

day08b :: Map (Int, Int) Integer -> Int
day08b m = let
  scenicScore p = product $ map (score p) [(-1,0),(1,0),(0,-1),(0,1)]
    where 
      h = m Map.! p
      score p d = case Map.lookup (p |+| d) m of
        Just k | k < h  -> 1 + score (p |+| d) d
        Just k          -> 1 -- tall tree
        _               -> 0 -- out of bounds
  in maximum $ map scenicScore $ Map.keys m

--------------------------------------------------------------------------------
-- DAY 9

parse09 :: Parser [(Int,Int)]
parse09 = fmap (List.scanl' (|+|) (0,0) . concat) 
        $  (`sepBy` "\n") 
        $  flip List.replicate
       <$> choice ["U"$>(0,-1),"D"$>(0,1),"L"$>(-1,0),"R"$>(1,0)]
       <*> (space *> decimal)


catchup :: (Int,Int) -> (Int,Int) -> (Int,Int)
catchup h t = let (dx,dy) = h |-| t in t |+|
  if abs dx < 2 && abs dy < 2 
    then (0,0)
    else (signum dx, signum dy)

day09a :: [(Int,Int)] -> Int
day09a = length . nub . List.scanl' (flip catchup) (0,0)

day09b :: [(Int,Int)] -> Int
day09b = length . nub . map last . List.scanl' updateWorm (replicate 9 (0,0))
  where updateWorm ts h = tail $ List.scanl' catchup h ts

--------------------------------------------------------------------------------
-- DAY 10 🎂

parse10 :: Parser [(Int,Int)]
parse10 = ((2,) <$> ("addx " *> signed decimal) <|> "noop" $> (1,0) ) 
    `sepBy` endOfLine

xsums :: [(Int,Int)] -> [Int]
xsums is = List.scanl' (+) 1 
  $ concatMap (\(x,n) -> replicate (x-1) 0 ++ [n]) $ is

day10a :: [(Int,Int)] -> Int
day10a is = sum $ (\n -> n * (xsums is !! n)) <$> [20,60,100,140,180,220]

day10b :: [(Int,Int)] -> [String]
day10b is = chunksOf 40
  $ zipWith (\n x -> bool ' ' '█' $ abs (n`mod`40 - x) < 2) [0..239] (xsums is) 

instance From [String] String where from = List.concatMap ('\n':)