module Countdown where

import Data.List
import Data.Maybe
import Control.Parallel

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

permute :: Eq a => [a] -> [[a]]
permute xs = concatMap permuteUsing $ nub xs
  where
    permuteUsing x = [x] : map (x:) (permute $ delete x xs)

expressions :: [Expression] -> [Expression]
expressions [] = []
expressions xs@[_] = xs
expressions xs = concatMap (expressionsFrom . (`splitAt` xs)) [1 .. length xs - 1]
  where
    expressionsFrom :: ([Expression], [Expression]) -> [Expression]
    expressionsFrom (leftOperands, rightOperands) =
      concatMap (combinations combiners) $ expressions rightOperands
      where
        combiners = concatMap combinersUsing $ expressions leftOperands

type Combiner = Expression -> Maybe Expression

combinations :: [Combiner] -> Expression -> [Expression]
combinations xs right = mapMaybe ($ right) xs

combinersUsing :: Expression -> [Combiner]
combinersUsing left = mapMaybe (`combinerUsing` left) [minBound :: Operation ..]

combinerUsing :: Operation -> Expression -> Maybe Combiner
combinerUsing op@Add left = Just $ makeExpression op left
combinerUsing op@Subtract left =
  if lv < 3 then Nothing else Just $ makeExpression op left
  where
    lv = value left
combinerUsing op left =
  if lv == 1 then Nothing else Just $ makeExpression op left
  where
    lv = value left

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

findBest :: Int -> Expression -> Maybe Expression -> Maybe Expression
findBest _ e1 Nothing = Just e1
findBest target e1 (Just e2) = Just $ if ordering == LT then e1 else e2
  where
    getters = [differenceFrom target, count, parenCount]
    ordering = foldMap (compareBy e1 e2) getters
    compareBy x y f = compare (f x) (f y)

differenceFrom :: Int -> Expression -> Int
differenceFrom target expr = abs (target - value expr)

data Priority = Low | High | Atomic deriving (Eq, Ord)

class Prioritizable a where
  priority :: a -> Priority

data Operation = Add | Subtract | Multiply | Divide deriving (Enum, Eq, Bounded)

instance Show Operation where
  show Add = "+"
  show Subtract = "-"
  show Multiply = "*"
  show Divide = "/"

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
commutative = (`elem` [Add, Multiply])

data Expression = NumberExpression Int | ArithmeticExpression Expression Operation Expression

value :: Expression -> Int
value (NumberExpression n) = n
value (ArithmeticExpression left op right) = eval op left right

count :: Expression -> Int
count (NumberExpression _) = 1
count (ArithmeticExpression left _ right) = sum $ map count [left, right]

numbersUsed :: Expression -> [Int]
numbersUsed (NumberExpression n) = [n]
numbersUsed (ArithmeticExpression left _ right) = concatMap numbersUsed [left, right]

parenCount :: Expression -> Int
parenCount (NumberExpression _) = 0
parenCount e@(ArithmeticExpression left _ right) = parens + nestedParens
  where
    parens = length $ elemIndices True $ [leftParens, rightParens] <*> [e]
    nestedParens = sum (map parenCount [left, right])

instance Eq Expression where
  a == b = value a == value b

instance Prioritizable Expression where
  priority (NumberExpression _) = Atomic
  priority (ArithmeticExpression _ op _) = priority op

instance Show Expression where
  show (NumberExpression n) = show n
  show e@(ArithmeticExpression left op right) =
    unwords $ intersperse (show op) operands
    where
      operands = zipWith parensIf [left, right] $ [leftParens, rightParens] <*> [e]

leftParens :: Expression -> Bool
leftParens (NumberExpression _) = False
leftParens (ArithmeticExpression left op _) = parens left op Nothing

rightParens :: Expression -> Bool
rightParens (NumberExpression _) = False
rightParens (ArithmeticExpression _ op right) = parens right op $ Just $ commutative op

parens :: Expression -> Operation -> Maybe Bool -> Bool
parens operand op opCommutative
  | operandPriority == opPriority = fromMaybe False opCommutative
  | otherwise = operandPriority < opPriority
  where
    operandPriority = priority operand
    opPriority = priority op

parensIf :: (Show a) => a -> Bool -> String
parensIf e withParens = if withParens then intercalate (show e) ["(", ")"] else show e

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