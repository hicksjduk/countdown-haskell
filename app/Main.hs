{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import Countdown
import Data.Bifunctor
import Data.Char
import Data.List
import System.Environment
import System.Random
import Utils

main = do
  args <- getArgs
  rand <- getStdGen
  either putStrLn solveIt $ do
    numArgs <- argsToNumeric args
    numericToNumbers rand numArgs

argsToNumeric :: [String] -> Either String [Int]
argsToNumeric [] = Left "Must specify at least one argument"
argsToNumeric xs
  | all (all isDigit) xs = Right $ map (read :: String -> Int) xs
  | otherwise = Left "All arguments must be non-negative integers"

numericToNumbers :: RandomGen a => a -> [Int] -> Either String [Int]
numericToNumbers rand [x]
  | x > 4 = Left "Number of big numbers must be in the range 0 to 4"
  | otherwise = Right $ randomNumbers rand x
numericToNumbers _ nums@(target : numbers)
  | (not . validTarget) target = Left "Target must be in the range 100 to 999 inclusive"
  | (not . validNumbers) numbers = Left "Numbers must include up to 2 each of the numbers 1 to 10 and up to 1 each of 25, 50, 75 and 100"
  | otherwise = Right nums

solveIt :: [Int] -> IO ()
solveIt (target : numbers) = do
  putStrLn $ unwords ["Solving with target number:", show target, "and source numbers:", show numbers]
  putStrLn $
    maybe
      "No solution found"
      (\e -> unwords [show e, "=", show $ value e])
      $ solve target numbers

validTarget :: Int -> Bool
validTarget n = n >= fst targetRange && n <= snd targetRange

validNumbers :: [Int] -> Bool
validNumbers ns = sort ns `isSubsequenceOf` (smallNumbers ++ bigNumbers)

targetRange = (100, 999)

bigNumbers = [(* 25)] <*> [1 .. 4]

smallNumbers = concat $ [replicate 2] <*> [1 .. 10]

randomNumbers :: RandomGen a => a -> Int -> [Int]
randomNumbers rand bigOnes = target : map fst (big ++ small)
  where
    (target, r1) = randomR targetRange rand
    small = take (6 - bigOnes) $ randomise r1 smallNumbers
    r2 = snd $ last small
    big = take bigOnes $ randomise r2 bigNumbers
    
randomise :: RandomGen a => a -> [b] -> [(b, a)]
randomise _ [] = []
randomise rand [x] = [(x, rand)]
randomise rand xs = (xs !! i, r1) : randomise r1 (deleteAt i xs)
  where
    (i, r1) = randomR (0, length xs - 1) rand
