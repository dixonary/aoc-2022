
module Main where

import qualified Aoc
import System.Environment (getArgs)
import Language.Haskell.TH
import Control.Monad (replicateM)
import Data.Char (isDigit)
import Witch

import Data.Maybe
import Data.Bool
import Control.Exception
import Data.Either

import Data.String.Interpolate

import Data.Map qualified as Map
import Data.Map (Map)

import Data.Attoparsec.Text 

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
instance From String Int where from = read
instance From Int String where from = show

--------------------------------------------------------------------------------
-- Day parsers can be direct functions or an Attoparser Parser
class IsParser a p where
  parsePart :: p -> String -> a

instance IsParser a (String -> a) where
  parsePart p = p

instance IsParser a (Parser a) where
  parsePart p = either error id . parseOnly p . from

--------------------------------------------------------------------------------


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
            (Just r, Just t) -> ([| ((n,p), AocPart ($(varE t) . parsePart $(varE r))) |])
        
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