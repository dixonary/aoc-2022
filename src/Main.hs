
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


data AocPart = forall a b . (AocFrom a, AocTo b) => AocPart (a -> b)

class AocTo a where aocTo :: a -> String
class AocFrom a where aocFrom :: String -> a
  
instance From a String => AocTo a   where aocTo   = from
instance From String a => AocFrom a where aocFrom = from

instance From String Integer where from = read
instance From Integer String where from = show

allParts :: Map (Integer, String) AocPart
allParts = Map.fromList $(do
    let
      lpad n str = let s = show str in replicate (n - length s) '0' ++ s 

      allPairs :: [(Integer, String)] -> Q [Q Exp]
      allPairs [] = pure []
      allPairs ((n,p):rest) = do
        lookupValueName ("Aoc.day" ++ (lpad 2 n) ++ p) >>= \case
          Nothing -> allPairs rest
          Just f' -> ([| ((n,p), AocPart $(varE f')) |] :) <$> allPairs rest
                
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
    runPart p i = do
      case Map.lookup (day, p) allParts of
        Nothing -> putStrLn $ "No part " ++ p ++ " for day " ++ lpad 2 day
        Just (AocPart f) -> do
          putStrLn $ "Part " ++ p ++ ": " ++ aocTo (f (aocFrom i))

  input <- readFileM $ "input/day" ++ lpad 2 day

  case input of
    Nothing -> putStrLn $ "No input found for day " ++ lpad 2 day
    Just i -> case part of
      Nothing -> do
        runPart "a" i
        runPart "b" i
      Just p -> runPart p i