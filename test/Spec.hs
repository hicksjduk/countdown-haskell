import Countdown
import Data.List
import Data.Maybe
import Test.Hspec
import Utils

main = do
  testExact 834 [10, 9, 8, 7, 6, 5] 5
  testExact 378 [50, 7, 4, 3, 2, 1] 3
  testExact 493 [50, 25, 4, 3, 2, 4] 6
  testExact 803 [50, 4, 9, 6, 6, 1] 6
  testExact 827 [25, 8, 5, 8, 1, 2] 6
  testExact 401 [10, 4, 5, 2, 3, 3] 6
  testExact 100 [100, 4, 5, 2, 3, 3] 1
  testNotExact 954 [50, 75, 25, 100, 5, 8] 955 5
  testNoSolution 999 [1, 2, 3, 4, 5, 6]

testExact :: Int -> [Int] -> Int -> IO ()
testExact target numbers expectedCount = hspec $ do
  describe (unwords ["Exact match:", show target, show numbers]) $ do
    let solution = solve target numbers
    checkSolutionFound solution
    let e = fromJust solution
    it "Matches target" $ do
      value e `shouldBe` target
    checkCount expectedCount e
    checkNumbersUsed numbers e

checkSolutionFound :: Maybe Expression -> SpecWith (Arg Expectation)
checkSolutionFound solution = it "Solution found" $ do
  solution `shouldSatisfy` isJust

checkCount :: Int -> Expression -> SpecWith (Arg Expectation)
checkCount expected expression = it (unwords ["Uses", show expected, "number(s)"]) $ do
  count expression `shouldBe` expected

checkNumbersUsed :: [Int] -> Expression -> SpecWith (Arg Expectation)
checkNumbersUsed numbers expression = it "Only uses some or all source numbers" $ do
  numbersUsed expression `shouldSatisfy` (`isSubsetOf` numbers)

testNotExact :: Int -> [Int] -> Int -> Int -> IO ()
testNotExact target numbers expectedAnswer expectedCount = hspec $ do
  describe (unwords ["Non-exact match:", show target, show numbers]) $ do
    let solution = solve target numbers
    checkSolutionFound solution
    let e = fromJust solution
    it (unwords ["Value of answer is", show expectedAnswer]) $ do
      value e `shouldBe` expectedAnswer
    checkCount expectedCount e
    checkNumbersUsed numbers e

testNoSolution :: Int -> [Int] -> IO ()
testNoSolution target numbers = hspec $ do
  describe (unwords ["No solution:", show target, show numbers]) $ do
    let solution = solve target numbers
    it "No solution found" $ do
      solution `shouldSatisfy` isNothing
