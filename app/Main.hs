module Main where

import Countdown
import Data.Char
import System.Environment

main = do
   args <- getArgs
   if and [all isDigit arg | arg <- args] then solveIt $ [read a::Int | a <- args]
   else putStrLn "Must only specify numbers"

solveIt (target:numbers)  = do
    putStrLn (join " " ["Solving with target number:", show target, "and source numbers:", show numbers])
    putStrLn (show $ Solution(solve target numbers))
