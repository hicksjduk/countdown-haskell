module Utils where

import Control.Parallel

-- |
-- Performs the specified fold on the supplied list, using parallel processing
-- for performance. The list is split into chunks, each of which is folded
-- separately and in parallel, and the results are combined to produce an
-- overall result.
-- Parameter 1 is the chunk size to use.
-- Parameter 2 is the fold.
-- Parameter 3 is a function that combines the results of folding two chunks.
-- Parameter 4 is the list.
foldParallel :: Int -> ([a] -> b) -> (b -> b -> b) -> [a] -> b
foldParallel _ fold _ [] = fold []
foldParallel chunkSize fold combine xs = par lf $ combine lf rf
  where
    (left, right) = splitAt chunkSize xs
    lf = fold left
    rf = foldParallel chunkSize fold combine right

-- |
-- Gets a list of all the distinct values in the input list. The order of the
-- values is the same as the order of their first occurrence in the input.
-- Parameter 1 is the list.
distinct :: Eq a => [a] -> [a]
distinct = distinctBy id

-- |
-- Gets a list of the values in the input list which have distinct keys (as
-- returned by the key generator function). Only the first value for each key
-- is included, in the same order as they occur in the input.
-- Parameter 1 is the key generator function.
-- Parameter 2 is the list.
distinctBy :: Eq b => (a -> b) -> [a] -> [a]
distinctBy keyGen xs = distinctBy' keyGen [] xs
  where
    distinctBy' :: Eq b => (a -> b) -> [b] -> [a] -> [a]
    distinctBy' _ _ [] = []
    distinctBy' keyGen usedKeys (x : xs)
      | key `elem` usedKeys = distinctBy' keyGen usedKeys xs
      | otherwise = x : distinctBy' keyGen (key : usedKeys) xs
      where
        key = keyGen x

-- |
-- Gets a copy of the input list, excluding the value at the specified index.
-- If the index is not a valid index for the list, the whole list is returned.
-- Parameter 1 is the index.
-- Parameter 2 is the list.
allExcept :: Int -> [a] -> [a]
allExcept _ [] = []
allExcept i lst@(x : xs)
  | i < 0 = lst
  | i == 0 = xs
  | otherwise = x : allExcept (i -1) xs

-- |
-- Gets the number of times the specified value occurs in the specified list.
-- Parameter 1 is the value.
-- Parameter 2 is the list.
occurrences :: Eq a => a -> [a] -> Int
occurrences x = countIf (==x)

-- |
-- Gets the number of items in the specified list that satisfy the supplied predicate.
-- Parameter 1 is the predicate.
-- Parameter 2 is the list.
countIf :: (a -> Bool) -> [a] -> Int
countIf predicate xs = length $ filter predicate xs

-- |
-- Gets whether the first list is a subset of the second one. List A is a subset of list B 
-- if every distinct value in A occurs not more times in A than in B.
-- Note that: (a) the order of elements is irrelevant; (b) if the two lists contain the
-- same values they are both subsets of each other.
-- Parameter 1 is the candidate subset.
-- Parameter 2 is the candidate superset.
isSubsetOf :: Eq a => [a] -> [a] -> Bool
a `isSubsetOf` b = and [occurrences x a <= occurrences x b | x <- distinct a]