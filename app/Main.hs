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
  case validateArgs args of
    (Just m) -> putStrLn m
    _ -> case validateNumbers nums of
      (Just m) -> putStrLn m
      _ -> solveIt $ normalise nums rand
      where
        nums = [read a :: Int | a <- args]

normalise :: RandomGen a => [Int] -> a -> [Int]
normalise [bigNums] rand = randomNumbers rand bigNums
normalise nums _ = nums

solveIt :: [Int] -> IO ()
solveIt (target : numbers) = do
  putStrLn $ unwords ["Solving with target number:", show target, "and source numbers:", show numbers]
  case solve target numbers of
    (Just e) -> putStrLn $ unwords [show e, "=", show (value e)]
    _ -> putStrLn "No solution found"

validateArgs :: [String] -> Maybe String
validateArgs [] = Just "Must specify at least one argument"
validateArgs xs
  | all (all isDigit) xs = Nothing
  | otherwise = Just "All arguments must be numeric"

validateNumbers :: [Int] -> Maybe String
validateNumbers [x]
  | x < 0 || x > 4 = Just "Number of big numbers must be in the range 0 to 4"
  | otherwise = Nothing
validateNumbers nums@(target : numbers)
  | (not . validTarget) target = Just "Target must be in the range 100 to 999 inclusive"
  | (not . validNumbers) numbers = Just "Numbers must include up to 2 each of the numbers 1 to 10 and up to 1 each of 25, 50, 75 and 100"
  | otherwise = Nothing

validTarget :: Int -> Bool
validTarget n = n >= 100 && n <= 999

validNumbers :: [Int] -> Bool
validNumbers ns = all (`validNumber` ns) $ distinct ns

validNumber :: Int -> [Int] -> Bool
validNumber n ns
  | n < 1 = False
  | n > 100 = False
  | n <= 10 = occurrences n ns <= 2
  | n `mod` 25 == 0 = occurrences n ns == 1
  | otherwise = False

randomNumbers :: RandomGen a => a -> Int -> [Int]
randomNumbers rand bigOnes =
  concat
    [ take 1 (randomRs (100, 999) rand),
      take bigOnes (randomFrom rand bigNumbers),
      take (6 - bigOnes) (randomFrom rand smallNumbers)
    ]
  where
    bigNumbers = [n * 25 | n <- [1 .. 4]]
    smallNumbers = concat [[n, n] | n <- [1 .. 10]]

randomFrom :: RandomGen a => a -> [Int] -> [Int]
randomFrom _ [] = []
randomFrom rand xs = take size [xs !! i | i <- distinct $ randomRs (0, size - 1) rand]
  where
    size = length xs