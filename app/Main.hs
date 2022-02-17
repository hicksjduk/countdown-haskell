{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import Countdown
import Data.Char
import Data.List
import Data.Bifunctor
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
  | all (all isDigit) xs = Right $ map (read::String->Int) xs
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
validTarget n = n >= 100 && n <= 999

validNumbers :: [Int] -> Bool
validNumbers ns = all (`validNumber` ns) $ nub ns

validNumber :: Int -> [Int] -> Bool
validNumber n ns
  | n < 1 = False
  | n <= 10 = occurrences n ns <= 2
  | n > 100 = False
  | n `mod` 25 /= 0 = False
  | otherwise = occurrences n ns == 1

bigNumbers = map (*25) [1 .. 4]
smallNumbers = concatMap (replicate 2) [1..10]

randomNumbers :: RandomGen a => a -> Int -> [Int]
randomNumbers rand bigOnes = target : take bigOnes big ++ take (6-bigOnes) small
  where
    (target, r1) = randomR (100, 999) rand
    (big, r2) = randomFrom r1 bigNumbers
    (small, _) = randomFrom r2 smallNumbers

randomFrom :: RandomGen a => a -> [b] -> ([b], a)
randomFrom rand [] = ([], rand)
randomFrom rand xs@[x] = (xs, rand)
randomFrom rand xs = (head right : res, r2)
   where
     indexRange = (0, length xs - 1)
     (i, r1) = randomR indexRange rand
     (left, right) = splitAt i xs
     (res, r2) = randomFrom r1 (left ++ tail right)