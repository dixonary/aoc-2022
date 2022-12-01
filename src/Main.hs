
module Main where

import qualified Aoc
import System.Environment (getArgs)
import Language.Haskell.TH
import Control.Monad (replicateM)
import Data.Char (isDigit)
import Witch
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe
import Data.Bool
import Control.Exception
import Data.Either

import Data.String.Interpolate

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Usage: ./aoc <day>[a|b]"
    toRun:_ -> do
      let (n,p) = span isDigit toRun
      let day = read @Integer n
      case p of
        p | p == "a" || p == "b" -> do
          runDay day $ Just p
        _ -> do
          putStrLn $ "Running day " ++ lpad 2 day
          putStrLn $ "=============="
          runDay day Nothing


data AocPart = forall b . (From b String) => 
    AocPart (String -> b) 
  | NoParser 
  | NoPart

instance From String Integer where from = read
instance From Integer String where from = show

-- Needed to get around limitations of quasiquoters
toString :: From b String => b -> String
toString = from


allParts :: Map (Integer, String) AocPart
allParts = Map.fromList $(do
    let
      lpad n str = let s = show str in replicate (n - length s) '0' ++ s


      allPairs :: [(Integer, String)] -> Q [Q Exp]
      allPairs [] = pure []
      allPairs ((n,p):rest) = do
          
        parser <- lookupValueName ("Aoc.parse" ++ lpad 2 n)
        part   <- lookupValueName ("Aoc.day" ++ (lpad 2 n) ++ p)

        rest <- allPairs rest

        let 
          cur = case (parser,part) of
            (Nothing,_)      -> ([| ((n,p), NoParser) |])
            (Just r,Nothing) -> ([| ((n,p), NoPart) |])
            (Just r, Just t) -> ([| ((n,p), AocPart ($(varE t) . $(varE r))) |])
        
        pure (cur:rest)
                
    listE =<< allPairs [ (x,y) | x <- [1..25], y <- ["a","b"] ]
  )

lpad :: Show a => Int -> a -> [Char]
lpad n str = let s = show str in replicate (n - length s) '0' ++ s 

runDay :: Integer -> Maybe String -> IO ()
runDay day part = do
  let
    readFileM :: FilePath -> IO (Maybe String)
    readFileM p = (Just <$> readFile p) `catch` 
      ((\e -> pure Nothing) :: IOException -> IO (Maybe String))

    runPart :: String -> String -> IO ()
    runPart p input = do
      putStrLn $ case Map.lookup (day, p) allParts of
        Nothing          -> [i|Could not find day #{lpad 2 day} in data|]
        Just NoParser    -> [i|No parser for day #{lpad 2 day}|]
        Just NoPart      -> [i|No part #{p} for day #{lpad 2 day}|]
        Just (AocPart f) -> [i|Part #{p}: #{toString (f input)}|]

  input <- readFileM $ "input/day" ++ lpad 2 day

  case input of
    Nothing -> putStrLn $ "No input found for day " ++ lpad 2 day
    Just i -> case part of
      Nothing -> do
        runPart "a" i
        runPart "b" i
      Just p -> runPart p i