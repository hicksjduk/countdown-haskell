module Countdown where

import Control.Parallel
import Data.Maybe
import Utils

solve :: Int -> [Int] -> Maybe Expression
solve target [] = Nothing
solve target xs = parallelFold (findBest target) $ map Just $ allExpressions $ map NumberExpression xs

allExpressions :: [Expression] -> [Expression]
allExpressions xs = concatMap expressions $ permute xs

expressions :: [Expression] -> [Expression]
expressions [] = []
expressions [x] = [x]
expressions xs = concatMap (expressionsFrom . (`splitAt` xs)) $ take (length xs - 1) [1 ..]
  where
    expressionsFrom :: ([Expression], [Expression]) -> [Expression]
    expressionsFrom (leftOperands, rightOperands) = concat [combinations combiners r | r <- expressions rightOperands]
      where
        combiners = concatMap combinersUsing (expressions leftOperands)

permute :: Eq a => [a] -> [[a]]
permute [] = []
permute [x] = [[x]]
permute xs = concat [permuteAt i xs | i <- uniqueIndices]
  where
    uniqueIndices = distinctBy (xs !!) $ take (length xs) [0 ..]
    permuteAt :: Eq a => Int -> [a] -> [[a]]
    permuteAt n xs = [x : suffix | suffix <- [] : permute others]
      where
        x = xs !! n
        others = allExcept n xs

combinersUsing :: Expression -> [Expression -> Maybe Expression]
combinersUsing left = [fromJust c | c <- filter isJust [combinerUsing op left | op <- [Add ..]]]

combinations :: [Expression -> Maybe Expression] -> Expression -> [Expression]
combinations xs right = catMaybes ([c right | c <- xs])

makeExpression :: Operation -> Expression -> Expression -> Maybe Expression
makeExpression op@Add left right = Just (ArithmeticExpression left op right)
makeExpression op@Subtract left right =
  if rv >= lv || rv * 2 == lv then Nothing else Just (ArithmeticExpression left op right)
  where
    lv = value left
    rv = value right
makeExpression op@Multiply left right =
  if rv == 1 then Nothing else Just (ArithmeticExpression left op right)
  where
    rv = value right
makeExpression op@Divide left right =
  if rv == 1 || lv `mod` rv /= 0 || rv ^ 2 == lv then Nothing else Just (ArithmeticExpression left op right)
  where
    lv = value left
    rv = value right

combinerUsing :: Operation -> Expression -> Maybe (Expression -> Maybe Expression)
combinerUsing op@Add left = Just (makeExpression op left)
combinerUsing op@Subtract left =
  if lv < 3 then Nothing else Just (makeExpression op left)
  where
    lv = value left
combinerUsing op left =
  if lv == 1 then Nothing else Just (makeExpression op left)
  where
    lv = value left

class Nillable a where
  nil :: a

instance Nillable (Maybe a) where
  nil = Nothing

parallelFold :: Nillable a => (a -> a -> a) -> [a] -> a
parallelFold _ [] = nil
parallelFold f xs = par lf $ f lf rf
  where
    (left, right) = splitAt 1000 xs
    lf = foldr f nil left
    rf = parallelFold f right

findBest :: Int -> Maybe Expression -> Maybe Expression -> Maybe Expression
findBest _ Nothing Nothing = Nothing
findBest target j@(Just e) Nothing = if differenceFrom target e > 10 then Nothing else j
findBest target Nothing j@(Just e) = findBest target j Nothing
findBest target j1@(Just e1) j2@(Just e2)
  | min diff1 diff2 > 10 = Nothing
  | diff1 < diff2 = j1
  | diff1 > diff2 = j2
  | count e1 <= count e2 = j1
  | otherwise = j2
  where
    diff1 = differenceFrom target e1
    diff2 = differenceFrom target e2

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

instance Eq Expression where
  a == b = value a == value b

instance Prioritizable Expression where
  priority (NumberExpression _) = Atomic
  priority (ArithmeticExpression _ op _) = priority op

instance Show Expression where
  show (NumberExpression n) = show n
  show (ArithmeticExpression left op right) =
    join
      " "
      [ parensIf (priority left < priority op) (show left),
        symbol op,
        parensIf
          (priority op > priority right || priority op == priority right && not (commutative op))
          (show right)
      ]

parensIf :: Bool -> String -> String
parensIf False s = s
parensIf True s = join s ["(", ")"]
