module Utils where

import Control.Parallel
import Data.List

-- |
-- Performs the specified fold on the supplied list, using parallel processing
-- for performance. The list is split into chunks, each of which is folded
-- separately and in parallel, and the results are combined to produce an
-- overall result.
-- Parameter 1 is the chunk size to use.
-- Parameter 2 is the fold. Note that this must be able to be applied to an empty list.
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
-- Gets a copy of the input list, excluding the value at the specified index.
-- If the index is not a valid index for the list, the whole list is returned.
-- Parameter 1 is the index.
-- Parameter 2 is the list.
deleteAt :: Int -> [a] -> [a]
deleteAt _ [] = []
deleteAt 0 (_ : xs) = xs
deleteAt i lst@(x : xs) = if i < 0 then lst else x : deleteAt (i - 1) xs

-- |
-- Gets the number of times the specified value occurs in the specified list.
-- Parameter 1 is the value.
-- Parameter 2 is the list.
occurrences :: Eq a => a -> [a] -> Int
occurrences x = countIf (== x)

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
isSubsetOf :: Ord a => [a] -> [a] -> Bool
a `isSubsetOf` b = sort a `isSubsequenceOf` sort b