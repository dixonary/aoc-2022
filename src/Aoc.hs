module Aoc where

import Prelude hiding (takeWhile)

import Witch ( From(..) )

import Data.Char


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

import Data.Vector qualified as Vector
import Data.Vector (Vector)

import Data.Vector.Mutable qualified as MVector
import Data.Vector.Mutable (MVector)

import Data.Sequence qualified as Seq
import Data.Sequence (Seq, (|>), (<|), ViewL(..))


import Data.Attoparsec.Text hiding (take, sepBy, sepBy1, count)
import Control.Applicative.Combinators
    ( (<|>), count, sepBy, sepBy1, between )
import Data.Char (isUpper)
import Utils.Parsers as P
import Utils.Utils as U

import Data.Ord
import Data.Bifunctor
import Data.Functor
import Data.Foldable
import Data.Function 
import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.STRef
import Control.Monad.Loops

import Data.Maybe
import Data.Bool
import Data.Tuple

import System.FilePath

import Debug.Trace

import Text.Pretty.Simple

import Text.Show.Functions

--------------------------------------------------------------------------------
-- Utilibobs

both :: (a -> b) -> (a,a) -> (b,b)
both f (x,y) = (f x, f y)

takeToEnd :: From Text a => Parser a
takeToEnd = (from <$> takeTill (=='\n')) <* (void (char '\n') <|> endOfInput)

skipLine :: Parser ()
skipLine = takeTill (=='\n') *> (void (char '\n') <|> endOfInput)

nextInt = skipWhile (notInClass "0123456789-") *> signed decimal

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

  passesX   = [[(x,y) | x <- l ... r] | y <- t ... b]
  passesY   = [[(x,y) | y <- t ... b] | x <- l ... r]
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

--------------------------------------------------------------------------------
-- DAY 11

type Monkey = (Seq Int, (Int -> Int), Int, Int, Int)

parse11 :: Parser (Map Int Monkey)
parse11 = fmap (Map.fromList . zip [0..]) $ (`sepBy` "\n\n") $ do
  nextInt *> skipWhile (not.isNumber)
  ns <- Seq.fromList <$> decimal `sepBy` ", "
  
  skipSpace *> "Operation: new = "
  lhs <- choice [ const <$> decimal, "old" $> id  ]
  bop <- choice [ " * "  $> (*)    , " + " $> (+) ]
  rhs <- choice [ const <$> decimal, "old" $> id  ]

  (ns, liftA2 bop lhs rhs,,,) <$> nextInt <*> nextInt <*> nextInt

computeMonkeys :: Int -> Int -> Map Int Monkey -> Int
computeMonkeys nRounds wf ms = runST $ do
  ns <- Vector.thaw $ Vector.fromList $ map (\(a,_,_,_,_) -> a) $ Map.elems ms
  is <- MVector.replicate (length ms) 0
  
  let 
    allMods = product $ map (\(_,_,m,_,_) -> m) $ Map.elems ms

    runRound = forM_ [0..length ms-1] \n -> do
      let (_,op,m,m1,m2) = ms Map.! n
      -- Ends if the current monkey is out of elements, otherwise iterates
      fix \go -> Seq.viewl <$> MVector.read ns n >>= \case
        EmptyL -> pure ()
        h :< t -> do
          MVector.modify is succ n
          MVector.write ns n t
          let 
            h' = op h `div` wf `mod` allMods
            n' = if h' `mod` m == 0 then m1 else m2 
          MVector.modify ns (|> h') n'
          go

  replicateM nRounds runRound

  product . take 2 . List.sortOn negate . toList <$> Vector.freeze is


day11a :: Map Int Monkey -> Int
day11a = computeMonkeys 20 3
    
day11b :: Map Int Monkey -> Int
day11b = computeMonkeys 10000 1

--------------------------------------------------------------------------------
-- DAY 12

parse12 :: Parser (Map (Int,Int) Char)
parse12 = coordinateParser (Just) 0

flood :: Map (Int, Int) Char 
      -> (Int,Int) -- coord from which paths are measured
      -> Map (Int,Int) Int
flood m end = flood' 0 (Map.empty, Set.singleton end)
  where 
  flood' k (fm, frontier)
    | Set.null frontier = fm
    | otherwise         = let
        frontier' = Set.fromList [
          y
          | x <- Set.elems frontier
          , dx <- [(-1,0),(1,0),(0,-1),(0,1)]
          , let y = x |-| dx
          , Just ky <- [Map.lookup y m]
          , Just kx <- [Map.lookup x m]
          ,    (kx == 'E' && ky == 'z') 
            || (ky == 'S')
            || (kx /= 'E' && ord kx - ord ky <= 1)
          ] Set.\\ Map.keysSet fm'
        fm' :: Map (Int,Int) Int
        fm' = Set.foldl' (\m y -> Map.insert y k m) fm frontier
        in flood' (k+1) (fm', frontier')

day12a :: Map (Int,Int) Char -> Int
day12a m = flood m endCoord Map.! startCoord
  where
    startCoord = fromJust $ lookup 'S' $ map swap $ Map.assocs m
    endCoord   = fromJust $ lookup 'E' $ map swap $ Map.assocs m

day12b :: Map (Int,Int) Char -> Int
day12b m = minimum $ Map.restrictKeys (flood m endCoord) as
  where
    as       = Map.keysSet $ Map.filter (=='a') m
    endCoord = fromJust $ lookup 'E' $ map swap $ Map.assocs m

--------------------------------------------------------------------------------
-- DAY 13

data Packet = N Int | L [Packet]

instance Eq Packet where 
  N x == N y =    x  ==    y
  N x == L y = [N x] ==    y
  L x == N y =    x  == [N y]
  L x == L y =    x  ==    y

instance Ord Packet where
  N x <= N y =    x  <=    y
  L x <= L y =    x  <=    y
  N x <= L y = [N x] <=    y
  L x <= N y =    x  <= [N y]

parse13 :: Parser [(Packet, Packet)]
parse13 = packet `around` "\n" `sepBy` "\n\n" 
  where packet =  N <$> decimal 
              <|> L <$> between "[" "]" (packet `sepBy` ",")

day13a :: [(Packet, Packet)] -> Int
day13a = sum . map fst . filter (uncurry (<) . snd). zip [1..]

day13b :: [(Packet, Packet)] -> Int
day13b ps = product $ map (fromJust . flip List.elemIndex l) [da,db]
  where
    l = sort $ da : db : concatMap (\(a,b) -> [a,b]) ps
    da = L [L [N 2]]
    db = L [L [N 6]]

--------------------------------------------------------------------------------
-- DAY 14

parse14 :: Parser [[(Int,Int)]]
parse14 = path `sepBy` "\n"
  where
    path = coord `sepBy` " -> "
    coord = signed decimal `around` ","

day14a :: [[(Int,Int)]] -> Int
day14a ps = runST simulate
  where
    [l,t,r,b] = [minimum,maximum] <*> ([map fst, map snd] <&> ($ concat ps))

    simulate :: forall s. ST s Int
    simulate = do
      world <- newSTRef Set.empty

      let isFree (x,y) = Set.notMember (x,y) <$> readSTRef world
          settle (x,y) = modifySTRef world $ Set.insert (x,y)

          addSand (x,y) 
            | x < l || x > r || y >= b = pure False
            | otherwise                = do
              w <- readSTRef world
              if 
                | (x  ,y+1) `Set.notMember` w -> addSand (x,y+1)
                | (x-1,y+1) `Set.notMember` w -> addSand (x-1, y+1) 
                | (x+1,y+1) `Set.notMember` w -> addSand (x+1, y+1)
                | otherwise                   -> settle (x,y) >> pure True

      -- Write the world
      mapM settle [ (a,b) | p <- ps, ((x,y), (x',y')) <- zip p (tail p)
                  , a <- x ... x', b <- y ... y' ]

      -- Keep adding sand until addSand returns false (leaves the space)
      length <$> whileM (addSand (500,0)) (pure ())   


day14b :: [[(Int,Int)]] -> Int
day14b ps = runST simulate
  where
    b = (+2) $ maximum $ map snd $ concat ps

    simulate :: forall s. ST s Int
    simulate = do
      world <- newSTRef Set.empty
      let isFree (x,y) = Set.notMember (x,y) <$> readSTRef world
          settle (x,y) = modifySTRef world $ Set.insert (x,y)
          
          addSand (x,y) = do
            w <- readSTRef world
            if 
              | y >= b - 1                  -> settle (x,y)
              | (x  ,y+1) `Set.notMember` w -> addSand (x  , y+1)
              | (x-1,y+1) `Set.notMember` w -> addSand (x-1, y+1) 
              | (x+1,y+1) `Set.notMember` w -> addSand (x+1, y+1)
              | otherwise                   -> settle (x,y)

      -- Write the world
      mapM settle [ (a,b) | p <- ps, ((x,y), (x',y')) <- zip p (tail p)
                  , a <- x ... x', b <- y ... y' ]

      -- Keep adding sand until (500,0) is no longer free
      length <$> whileM (isFree (500,0)) (addSand (500,0))
