module Utils where

import Data.Maybe
   
distinct :: Eq a => [a] -> [a]
distinct xs = distinctBy id xs

distinctBy :: Eq b => (a -> b) -> [a] -> [a]
distinctBy keyGen xs = distinctBy' keyGen [] xs
   where distinctBy' :: Eq b => (a -> b) -> [b] -> [a] -> [a]
         distinctBy' _ _ [] = []
         distinctBy' keyGen usedKeys (x:xs)
            | key `elem` usedKeys = distinctBy' keyGen usedKeys xs
            | otherwise = x : distinctBy' keyGen (key : usedKeys) xs
            where key = keyGen x

allExcept :: Int -> [a] -> [a]
allExcept _ [] = []
allExcept i (x:xs) = if i == 0 then xs else x : allExcept (i-1) xs

join :: String -> [String] -> String
join _ [] = ""
join _ (x:[]) = x
join s (x:xs) = x ++ s ++ join s xs

occurrences :: Eq a => a -> [a] -> Int
occurrences _ [] = 0
occurrences y (x:xs) = (if y == x then 1 else 0) + occurrences y xs

isSubsetOf :: Eq a => [a] -> [a] -> Bool
isSubsetOf super sub = and [occurrences x sub <= occurrences x super | x <- distinct sub]