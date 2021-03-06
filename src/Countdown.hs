module Countdown where

import Data.List
import Data.Maybe
import Data.Function
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

permute :: Eq a => [a] -> [[a]]
permute [] = []
permute [x] = [[x]]
permute xs = concatMap (`permuteAt` xs) uniqueIndices
  where
    uniqueIndices = nubBy ((==) `on` (xs!!)) $ take (length xs) [0 ..]
    permuteAt :: Eq a => Int -> [a] -> [[a]]
    permuteAt n xs = (xs !! n :) <$> [] : permute others
      where
        others = deleteAt n xs

expressions :: [Expression] -> [Expression]
expressions [] = []
expressions [x] = [x]
expressions xs = concatMap expressionsFrom $ (`splitAt` xs) <$> [1 .. length xs - 1]
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
findBest target e1 (Just e2) = Just $ maybe e1 mapBack $ comparison e1 e2
  where
    getters = [differenceFrom target, count, parenCount]
    comparers = (compare `on`) <$> getters
    comparison e1 e2 = find (/=EQ) $ ($ e2) <$> ($ e1) <$> comparers
    mapBack LT = e1
    mapBack _ = e2

differenceFrom :: Int -> Expression -> Int
differenceFrom target expr = abs (target - value expr)

data Priority = Low | High | Atomic deriving (Eq, Ord)

class Prioritizable a where
  priority :: a -> Priority

data Operation = Add | Subtract | Multiply | Divide deriving (Show, Enum, Eq, Bounded)

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
  priority op
    | op `elem` [Add, Subtract] = Low
    | otherwise = High

commutative :: Operation -> Bool
commutative op = op `elem` [Add, Multiply]

data Expression = NumberExpression Int | ArithmeticExpression Expression Operation Expression

value :: Expression -> Int
value (NumberExpression n) = n
value (ArithmeticExpression left op right) = eval op left right

count :: Expression -> Int
count (NumberExpression _) = 1
count (ArithmeticExpression left _ right) = sum $ count <$> [left, right]

numbersUsed :: Expression -> [Int]
numbersUsed (NumberExpression n) = [n]
numbersUsed (ArithmeticExpression left _ right) = concatMap numbersUsed [left, right]

parenCount :: Expression -> Int
parenCount (NumberExpression _) = 0
parenCount e@(ArithmeticExpression left _ right) =
  sum $ countIf ($e) [leftParens, rightParens] : map parenCount [left, right]

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
