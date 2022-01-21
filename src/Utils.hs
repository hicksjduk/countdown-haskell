module Utils where

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
-- Gets a string containing the elements of the specified list, interspersed
-- with the specified string.
-- Parameter 1 is the string to be interspersed.
-- Parameter 2 is the list.
join :: String -> [String] -> String
join _ [] = ""
join _ [x] = x
join s (x : xs) = x ++ s ++ join s xs

-- |
-- Gets the number of times the specified value occurs in the specified list.
-- Parameter 1 is the value.
-- Parameter 2 is the list.
occurrences :: Eq a => a -> [a] -> Int
occurrences _ [] = 0
occurrences y (x : xs) = (if y == x then 1 else 0) + occurrences y xs

-- |
-- Gets whether the second list is a subset of the first one, which is the case if every
-- distinct value in the second list occurs not more times in that list than in the first
-- one. Note that: (a) the order of elements is irrelevant; (b) if the two lists contain the
-- same values they are both subsets of each other.
-- Parameter 1 is the candidate superset.
-- Parameter 2 is the candidate subset.
isSubsetOf :: Eq a => [a] -> [a] -> Bool
isSubsetOf super sub = and [occurrences x sub <= occurrences x super | x <- distinct sub]