module Main where

import Countdown
import Data.Char
import System.Environment

main = do
   args <- getArgs
   case validateAndConvert args of
      (Message m) -> putStrLn m
      (Numbers n) -> solveIt n

solveIt (target:numbers)  = do
    putStrLn (join " " ["Solving with target number:", show target, "and source numbers:", show numbers])
    putStrLn (show $ Solution(solve target numbers))

data ValidationResult = Message String | Numbers [Int]

instance Show ValidationResult where
   show (Message m) = m
   
valid :: ValidationResult -> Bool
valid (Message _) = False
valid _ = True

validateAndConvert :: [String] -> ValidationResult
validateAndConvert [] = Message "Must specify at least one argument"
validateAndConvert args
   | or [any (not.isDigit) x | x <- args] = Message "All arguments must be numeric"
   | otherwise = validateNumbers [read x::Int | x <- args]
   
validateNumbers :: [Int] -> ValidationResult
validateNumbers (x:[]) = Message "One argument specified, not supported yet"
validateNumbers nums@(target:numbers)
   | (not.validTarget) target = Message "Target must be in the range 100 to 999 inclusive"
   | (not.validNumbers) numbers = Message "Numbers must include up to 2 each of the numbers 1 to 10 and up to 1 each of 25, 50, 75 and 100"
   | otherwise = Numbers nums
   
validTarget :: Int -> Bool
validTarget n = n >= 100 && n <= 999

validNumbers :: [Int] -> Bool
validNumbers ns = and [validNumber n ns | n <- ns]

validNumber :: Int -> [Int] -> Bool
validNumber n ns
   | n < 1 = False
   | n > 100 = False
   | n <= 10 = occurrences n ns <= 2
   | n `mod` 25 == 0 = occurrences n ns == 1
   | otherwise = False
   
occurrences :: Eq a => a -> [a] -> Int
occurrences _ [] = 0
occurrences y (x:xs) = (if y == x then 1 else 0) + occurrences y xs