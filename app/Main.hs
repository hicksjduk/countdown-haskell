{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import Countdown
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
  | all (all isDigit) xs = Right [read x :: Int | x <- xs]
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
validNumbers ns = all (`validNumber` ns) $ distinct ns

validNumber :: Int -> [Int] -> Bool
validNumber n ns
  | n < 1 = False
  | n <= 10 = occurrences n ns <= 2
  | n > 100 = False
  | n `mod` 25 /= 0 = False
  | otherwise = occurrences n ns == 1

bigNumbers = [n * 25 | n <- [1 .. 4]]
smallNumbers = concat [[n, n] | n <- [1 .. 10]]

randomNumbers :: RandomGen a => a -> Int -> [Int]
randomNumbers rand bigOnes = concat [target, big, small]
  where
    target = take 1 $ randomRs (100, 999) rand
    big = take bigOnes $ randomFrom rand bigNumbers
    small = take (6 - bigOnes) $ randomFrom rand smallNumbers

randomFrom :: RandomGen a => a -> [Int] -> [Int]
randomFrom _ [] = []
randomFrom rand xs = take size [xs !! i | i <- distinct $ randomRs (0, size - 1) rand]
  where
    size = length xs