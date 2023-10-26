{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import Countdown
import Data.Char
import Data.List
import System.Environment
import System.Random


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
  | (not . validNumbers) numbers = Left $ unwords 
        ["Numbers must include up to 2 each of the numbers 1 to 10",
         "and up to 1 each of 25, 50, 75 and 100"]
  | otherwise = Right nums

solveIt :: [Int] -> IO ()
solveIt (target : numbers) = do
  putStrLn $ unwords ["Solving with target number:", show target, 
    "and source numbers:", show numbers]
  putStrLn $
    maybe "No solution found" (\e -> unwords [show e, "=", show $ value e])
      $ solve target numbers

validTarget :: Int -> Bool
validTarget n = let (min, max) = targetRange in n >= min && n <= max

validNumbers :: [Int] -> Bool
validNumbers ns = sort ns `isSubsequenceOf` (smallNumbers ++ bigNumbers)

targetRange = (100, 999)

bigNumbers = map (* 25) [1 .. 4]

smallNumbers = concatMap (replicate 2) [1 .. 10]

randomNumbers :: RandomGen a => a -> Int -> [Int]
randomNumbers rand bigOnes = target : take 6 (take bigOnes big ++ small)
  where
    (target, r1) = randomR targetRange rand
    (big, r2) = shuffle r1 bigNumbers
    (small, _) = shuffle r2 smallNumbers
    
shuffle :: RandomGen a => a -> [b] -> ([b], a)
shuffle rand xs
  | length xs < 2 = (xs, rand)
  | otherwise = (xs !! i : ns, r2)
  where
    (i, r1) = randomR (0, length xs - 1) rand
    (ns, r2) = shuffle r1 $ deleteAt i xs

deleteAt :: Int -> [a] -> [a]
deleteAt i xs = case splitAt i xs of
  (_, []) -> xs
  (ys, _:zs) -> if i < 0 then xs else ys ++ zs