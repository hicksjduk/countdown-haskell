module Countdown where

import Data.List
import Data.Maybe
import Utils

solve :: Int -> [Int] -> Maybe Expression
solve target xs = foldParallel chunkSize folder combiner filteredExprs
  where
    chunkSize = 100
    folder = foldr (findBest target) Nothing
    combiner :: Maybe Expression -> Maybe Expression -> Maybe Expression
    combiner Nothing m2 = m2
    combiner (Just e1) m2 = findBest target e1 m2
    exprs = allExpressions $ map NumberExpression xs
    filteredExprs = filter ((<= 10) . differenceFrom target) exprs

allExpressions :: [Expression] -> [Expression]
allExpressions xs = concatMap expressions $ permute xs

expressions :: [Expression] -> [Expression]
expressions [] = []
expressions xs@[_] = xs
expressions xs = concatMap (expressionsFrom . (`splitAt` xs)) [1 .. length xs - 1]
  where
    expressionsFrom :: ([Expression], [Expression]) -> [Expression]
    expressionsFrom (leftOperands, rightOperands) =
      concatMap (combinations combiners) $ expressions rightOperands
      where
        combiners = concatMap combinersUsing (expressions leftOperands)

permute :: Eq a => [a] -> [[a]]
permute [] = []
permute xs@[_] = [xs]
permute xs = concatMap (`permuteAt` xs) uniqueIndices
  where
    uniqueIndices = distinctBy (xs !!) $ take (length xs) [0 ..]
    permuteAt :: Eq a => Int -> [a] -> [[a]]
    permuteAt n xs = map (x :) $ [] : permute others
      where
        x = xs !! n
        others = allExcept n xs

combinersUsing :: Expression -> [Expression -> Maybe Expression]
combinersUsing left = mapMaybe (`combinerUsing` left) [Add ..]

combinations :: [Expression -> Maybe Expression] -> Expression -> [Expression]
combinations xs right = mapMaybe ($ right) xs

makeExpression :: Operation -> Expression -> Expression -> Maybe Expression
makeExpression op@Add left right = Just $ ArithmeticExpression left op right
makeExpression op@Subtract left right
  | rv >= lv = Nothing
  | rv * 2 == lv = Nothing
  | otherwise = Just $ ArithmeticExpression left op right
  where
    lv = value left
    rv = value right
makeExpression op@Multiply left right
  | rv == 1 = Nothing
  | otherwise = Just $ ArithmeticExpression left op right
  where
    rv = value right
makeExpression op@Divide left right
  | rv == 1 = Nothing
  | lv `mod` rv /= 0 = Nothing
  | rv ^ 2 == lv = Nothing
  | otherwise = Just $ ArithmeticExpression left op right
  where
    lv = value left
    rv = value right

combinerUsing :: Operation -> Expression -> Maybe (Expression -> Maybe Expression)
combinerUsing op@Add left = Just $ makeExpression op left
combinerUsing op@Subtract left =
  if lv < 3 then Nothing else Just $ makeExpression op left
  where
    lv = value left
combinerUsing op left =
  if lv == 1 then Nothing else Just $ makeExpression op left
  where
    lv = value left

findBest :: Int -> Expression -> Maybe Expression -> Maybe Expression
findBest _ e1 Nothing = Just e1
findBest target e1 (Just e2) = if null better then Just e1 else head better
  where
    getters = [differenceFrom target, count, parenCount]
    better = filter isJust $ map (\g -> lesserBy g e1 e2) getters

lesserBy :: Ord b => (a -> b) -> a -> a -> Maybe a
lesserBy f x y
  | fx == fy = Nothing
  | otherwise = Just $ if fx < fy then x else y
  where
    fx = f x
    fy = f y

differenceFrom :: Int -> Expression -> Int
differenceFrom target expr = abs (target - value expr)

data Priority = Low | High | Atomic deriving (Eq, Ord)

class Prioritizable a where
  priority :: a -> Priority

data Operation = Add | Subtract | Multiply | Divide deriving (Show, Enum)

symbol :: Operation -> String
symbol Add = "+"
symbol Subtract = "-"
symbol Multiply = "*"
symbol Divide = "/"

eval :: Operation -> Expression -> Expression -> Int
eval Add a b = value a + value b
eval Subtract a b = value a - value b
eval Multiply a b = value a * value b
eval Divide a b = value a `div` value b

instance Prioritizable Operation where
  priority Add = Low
  priority Subtract = Low
  priority _ = High

commutative :: Operation -> Bool
commutative Add = True
commutative Multiply = True
commutative _ = False

data Expression = NumberExpression Int | ArithmeticExpression Expression Operation Expression

value :: Expression -> Int
value (NumberExpression n) = n
value (ArithmeticExpression left op right) = eval op left right

count :: Expression -> Int
count (NumberExpression _) = 1
count (ArithmeticExpression left _ right) = sum [count left, count right]

numbersUsed :: Expression -> [Int]
numbersUsed (NumberExpression n) = [n]
numbersUsed (ArithmeticExpression left _ right) = numbersUsed left ++ numbersUsed right

parenCount :: Expression -> Int
parenCount (NumberExpression _) = 0
parenCount e@(ArithmeticExpression left _ right) =
  sum [countIf ($e) [leftParens, rightParens], parenCount left, parenCount right]

instance Eq Expression where
  a == b = value a == value b

instance Prioritizable Expression where
  priority (NumberExpression _) = Atomic
  priority (ArithmeticExpression _ op _) = priority op

instance Show Expression where
  show (NumberExpression n) = show n
  show e@(ArithmeticExpression left op right) =
    unwords [parensLeft $ show left, symbol op, parensRight $ show right]
    where
      parensLeft = parensIf $ leftParens e
      parensRight = parensIf $ rightParens e

leftParens :: Expression -> Bool
leftParens (NumberExpression _) = False
leftParens (ArithmeticExpression left op _) = lPriority < oPriority
  where
    lPriority = priority left
    oPriority = priority op

rightParens :: Expression -> Bool
rightParens (NumberExpression _) = False
rightParens (ArithmeticExpression _ op right)
  | rPriority > oPriority = False
  | rPriority < oPriority = True
  | otherwise = (not . commutative) op
  where
    rPriority = priority right
    oPriority = priority op

parensIf :: Bool -> String -> String
parensIf False s = s
parensIf True s = intercalate s ["(", ")"]
