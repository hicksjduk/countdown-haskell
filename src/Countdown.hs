module Countdown where

import Data.List
import Data.Maybe
import Control.Parallel
import Data.Function
import Data.Monoid

solve :: Int -> [Int] -> Maybe Expression
solve target xs = 
  case mconcatParallel chunkSize exprMonoids of
    WithoutExpression -> Nothing
    WithExpression _ e -> Just e
  where
    chunkSize = 100
    exprs = allExpressions $ map NumberExpression xs
    filteredExprs = filter ((<= 10) . differenceFrom target) exprs
    exprMonoids = map (WithExpression target) filteredExprs

allExpressions :: [Expression] -> [Expression]
allExpressions xs = concatMap expressions $ permute xs

permute :: Eq a => [a] -> [[a]]
permute xs = 
  let permuteUsing x = [x] : map (x:) (permute $ delete x xs)
  in concatMap permuteUsing $ nub xs

expressions :: [Expression] -> [Expression]
expressions [] = []
expressions xs@[_] = xs
expressions xs = concatMap (expressionsFrom . (`splitAt` xs)) [1 .. length xs - 1]

expressionsFrom :: ([Expression], [Expression]) -> [Expression]
expressionsFrom (leftOperands, rightOperands) =
  let combiners = concatMap combinersUsing $ expressions leftOperands
  in concatMap (combinations combiners) $ expressions rightOperands
    

type Combiner = Expression -> Maybe Expression

combinations :: [Combiner] -> Expression -> [Expression]
combinations xs right = mapMaybe ($ right) xs

combinersUsing :: Expression -> [Combiner]
combinersUsing left = mapMaybe (`combinerUsing` left) [minBound :: Operation ..]

combinerUsing :: Operation -> Expression -> Maybe Combiner
combinerUsing op left = toMaybe (validForOperand op) $ makeExpression op left
  where
    validForOperand Add = True
    validForOperand Subtract = lv >= 3
    validForOperand _ = lv /= 1
    lv = value left

makeExpression :: Operation -> Expression -> Expression -> Maybe Expression
makeExpression op left right = toMaybe (validForOperands op) $ ArithmeticExpression left op right
  where
    validForOperands Add = True
    validForOperands Subtract = lv > rv && lv /= rv * 2
    validForOperands Multiply = rv /= 1
    validForOperands Divide = rv /= 1 && lv `mod` rv == 0 && lv /= rv ^ 2
    lv = value left
    rv = value right

toMaybe :: Bool -> a -> Maybe a
toMaybe False _ = Nothing
toMaybe True a = Just a

differenceFrom :: Int -> Expression -> Int
differenceFrom target expr = abs (target - value expr)

data Priority = Low | High | Atomic deriving (Eq, Ord)

class Prioritizable a where
  priority :: a -> Priority

comparePriority :: (Prioritizable a, Prioritizable b) => a -> b -> Ordering
comparePriority a b = compare (priority a) (priority b)

data Operation = Add | Subtract | Multiply | Divide deriving (Enum, Eq, Bounded)

instance Show Operation where
  show Add = "+"
  show Subtract = "-"
  show Multiply = "*"
  show Divide = "/"

eval :: Operation -> Int -> Int -> Int
eval Add = (+)
eval Subtract = (-)
eval Multiply = (*)
eval Divide = div

instance Prioritizable Operation where
  priority Add = Low
  priority Subtract = Low
  priority _ = High

commutative :: Operation -> Bool
commutative = (`elem` [Add, Multiply])

data Expression = NumberExpression Int | ArithmeticExpression Expression Operation Expression

value :: Expression -> Int
value (NumberExpression n) = n
value (ArithmeticExpression left op right) = eval op (value left) (value right)

numberCount :: Expression -> Int
numberCount (NumberExpression _) = 1
numberCount (ArithmeticExpression left _ right) = sum $ map numberCount [left, right]

numbersUsed :: Expression -> [Int]
numbersUsed (NumberExpression n) = [n]
numbersUsed (ArithmeticExpression left _ right) = concatMap numbersUsed [left, right]

parenCount :: Expression -> Int
parenCount (NumberExpression _) = 0
parenCount e@(ArithmeticExpression left _ right) = sum $ parens : nestedParens
  where
    parens = length $ filter id $ map ($ e) [leftParens, rightParens]
    nestedParens = map parenCount [left, right]

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
      operands = zipWith parensIf [left, right] $ map ($ e) [leftParens, rightParens]

leftParens :: Expression -> Bool
leftParens (NumberExpression _) = False
leftParens (ArithmeticExpression left op _) =
  case comparePriority left op of
    LT -> True
    _ -> False

rightParens :: Expression -> Bool
rightParens (NumberExpression _) = False
rightParens (ArithmeticExpression _ op right) =
  case comparePriority right op of
    LT -> True
    EQ -> not $ commutative op
    GT -> False

parensIf :: (Show a) => a -> Bool -> String
parensIf e False = show e
parensIf e True = intercalate (show e) ["(", ")"]

data ExpressionMonoid = WithoutExpression | WithExpression Int Expression deriving (Eq)

instance Ord ExpressionMonoid where
  compare WithoutExpression WithoutExpression = EQ
  compare WithoutExpression _ = GT
  compare _ WithoutExpression = LT
  compare (WithExpression target e1) (WithExpression _ e2) = comp e1 e2
    where
      criteria = [differenceFrom target, numberCount, parenCount]
      comp = foldMap (compare `on`) criteria

instance Monoid ExpressionMonoid where
  mempty = WithoutExpression

instance Semigroup ExpressionMonoid where  
  (<>) = min

mconcatParallel :: (Monoid m) => Int -> [m] -> m
mconcatParallel _ [] = mempty
mconcatParallel chunkSize xs = par m1 $ m1 <> m2
  where
    (left, right) = splitAt chunkSize xs
    m1 = mconcat left
    m2 = mconcatParallel chunkSize right