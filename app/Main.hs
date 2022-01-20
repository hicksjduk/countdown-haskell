module Main where

import Countdown
import Utils
import Data.Char
import System.Environment
import System.Random

main = do
   args <- getArgs
   rand <- getStdGen
   case validateAndConvert args rand of
      (Message m) -> putStrLn m
      (Numbers n) -> solveIt n

solveIt (target:numbers)  = do
    putStrLn (join " " ["Solving with target number:", show target, "and source numbers:", show numbers])
    putStrLn (printOut (solve target numbers))

printOut :: Maybe Expression -> String
printOut Nothing = "No solution found"
printOut (Just e) = join " = " [show e, show (value e)]

data ValidationResult = Message String | Numbers [Int]

validateAndConvert :: RandomGen a => [String] -> a -> ValidationResult
validateAndConvert [] _ = Message "Must specify at least one argument"
validateAndConvert args rand
   | or [any (not.isDigit) x | x <- args] = Message "All arguments must be numeric"
   | otherwise = validateNumbers [read x::Int | x <- args] rand
   
validateNumbers :: RandomGen a => [Int] -> a -> ValidationResult
validateNumbers (x:[]) rand
   | x < 0 || x > 4 = Message "Number of big numbers must be in the range 0 to 4"
   | otherwise = Numbers $ randomNumbers rand x
validateNumbers nums@(target:numbers) _
   | (not.validTarget) target = Message "Target must be in the range 100 to 999 inclusive"
   | (not.validNumbers) numbers = Message "Numbers must include up to 2 each of the numbers 1 to 10 and up to 1 each of 25, 50, 75 and 100"
   | otherwise = Numbers nums
   
validTarget :: Int -> Bool
validTarget n = n >= 100 && n <= 999

validNumbers :: [Int] -> Bool
validNumbers ns = all (`validNumber` ns) ns

validNumber :: Int -> [Int] -> Bool
validNumber n ns
   | n < 1 = False
   | n > 100 = False
   | n <= 10 = occurrences n ns <= 2
   | n `mod` 25 == 0 = occurrences n ns == 1
   | otherwise = False
   
randomNumbers :: RandomGen a => a -> Int -> [Int]
randomNumbers rand bigOnes = concat [take 1 (randomRs (100, 999) rand),
                                     take bigOnes (randomFrom rand bigNumbers),
                                     take (6-bigOnes) (randomFrom rand smallNumbers)]
   where bigNumbers = [n * 25 | n <- [1..4]]
         smallNumbers = concat [[n,n] | n <- [1..10]]

randomFrom :: RandomGen a => a -> [Int] -> [Int]
randomFrom rand [] = []
randomFrom rand xs = take (length xs) [xs!!i | i <- distinct (randomRs (0, length xs - 1) rand)]