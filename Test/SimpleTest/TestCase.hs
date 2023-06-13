{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}

module Test.SimpleTest.TestCase where

import Data.Fixed (Centi)
import qualified Data.List as L
import qualified Test.SimpleTest.Color as Color
import Test.SimpleTest.Expectation
import Text.Printf (printf)

data TestPoints a where
  TestPoints :: (Num a, Show a) => {testPointsObtained :: a, testPointsTotal :: a} -> TestPoints a

fullPoints :: (Num a, Show a) => a -> TestPoints a
fullPoints pts = TestPoints pts pts

noPoints :: (Num a, Show a) => a -> TestPoints a
noPoints pts = TestPoints 0 pts

resultToPoints :: TestResult -> TestPoints Centi
resultToPoints (Passed (TestCase {testCaseAssertions = assertions})) = fullPoints (totalPoints assertions)
resultToPoints (Failed _ assertionsResults) = earnedPoints assertionsResults
resultToPoints (Todo _ _ _ assertionsResults) = earnedPoints assertionsResults

totalPoints :: [Assertion] -> Centi
totalPoints assertions = sum (fmap assertionPoints assertions)

assertionResultToPoints :: AssertionResult -> TestPoints Centi
assertionResultToPoints (PassedAssertion a) = fullPoints pts where pts = assertionPoints a
assertionResultToPoints (FailedAssertion a) = noPoints pts where pts = assertionPoints a
assertionResultToPoints (TodoAssertion {todoAssertion = a}) = noPoints pts where pts = assertionPoints a

earnedPoints :: [AssertionResult] -> TestPoints Centi
earnedPoints = foldr ((<>) . assertionResultToPoints) mempty

instance Semigroup (TestPoints a) where
  (TestPoints obtainedA totalA) <> (TestPoints obtainedB totalB) = TestPoints (obtainedA + obtainedB) (totalA + totalB)

instance (Num a, Show a) => Monoid (TestPoints a) where
  mempty = TestPoints 0 0

instance Show (TestPoints a) where
  show (TestPoints obtained total) = printf "%s/%s" (show obtained) (show total)

data TestResult
  = Passed TestCase
  | Failed {failedTestCase :: TestCase, failedAssertions :: [AssertionResult]}
  | Todo {todoTestCase :: TestCase, todoMessage :: String, todoLocation :: String, todoAssertions :: [AssertionResult]}

data AssertionResult
  = PassedAssertion Assertion
  | FailedAssertion Assertion
  | TodoAssertion {todoAssertion :: Assertion, todoAssertionMessage :: String, todoAssertionLocation :: String}

instance Show TestResult where
  show (Passed TestCase {testCaseName = name, testCaseAssertions = assertions}) = printf "%s: %s" name (Color.green (show $ fullPoints (totalPoints assertions)))
  show (Failed TestCase {testCaseName = name, testCaseAssertions = assertions} assertionsResults) =
    printf "%s: %s" (Color.red name) (Color.red (show $ earnedPoints assertionsResults))
  show (Todo TestCase {testCaseName = name, testCaseAssertions = assertions} message location assertionsResults) =
    printf "%s: %s" name (Color.yellow $ printf "(%s) TODO: %s" (show $ earnedPoints assertionsResults) message)

showDetails :: TestResult -> String
showDetails (Failed TestCase {testCaseName = name, testCaseAssertions = assertions} assertionsResults) =
  printf "%s: %s%s" (Color.red name) (Color.red (show $ earnedPoints assertionsResults)) details
  where
    details = printf "\n%s\n%s\n%s" startline (printAssertionResultList assertionsResults) endline :: String
    startline = printf "%s" (replicate 80 'v') :: String
    endline = printf "%s" (replicate 80 '^') :: String
showDetails (Todo (TestCase {testCaseName = name}) message location assertionsResults) =
  printf "%s: %s" name (Color.yellow $ printf "(%s) TODO: %s%s" (show $ earnedPoints assertionsResults) message details)
  where
    details = printf "\n%s\n%s\n%s" startline location endline :: String
    startline = printf "%s" (replicate 80 'v') :: String
    endline = printf "%s" (replicate 80 '^') :: String
showDetails _ = printf ""

instance Show AssertionResult where
  show (FailedAssertion (Assertion {assertionFeedback = feedback, assertionExpectation = expectation})) = 
    printf "%s\n%s" (Color.yellow feedback) (show expectation)
  show _ = printf ""

printAssertionResultList :: [AssertionResult] -> String
printAssertionResultList [] = printf ""
printAssertionResultList (ar:ars)
  | null (show ar) = printf "%s" (printAssertionResultList ars)
  | null (printAssertionResultList ars) = printf "%s" (show ar)
  | otherwise = printf "%s\n%s" (show ar) (printAssertionResultList ars)

data Assertion where
  Assertion ::
    Expectation e =>
    { assertionExpectation :: e,
      assertionFeedback :: String,
      assertionPoints :: Centi
    } -> Assertion

data TestCase = TestCase { testCaseName :: String, testCaseAssertions :: [Assertion], enabled :: Bool }

data TestTree a where
  TestTreeNode :: Show a => {testGroupName :: String, testGroupChildren :: [TestTree a]} -> TestTree a
  TestTreeLeaf :: Show a => a -> TestTree a
  TestTreeEmpty :: Show a => TestTree a

instance Show a => Monoid (TestTree a) where
  mempty = TestTreeEmpty

instance Show a => Semigroup (TestTree a) where
  TestTreeEmpty <> any = any
  any <> TestTreeEmpty = any
  lf1@(TestTreeLeaf _) <> lf2@(TestTreeLeaf _) = TestTreeNode "" [lf1, lf2]
  (TestTreeNode n1 c1) <> lf2@(TestTreeLeaf _) = TestTreeNode n1 (c1 ++ [lf2])
  lf1@(TestTreeLeaf _) <> (TestTreeNode n2 c2) = TestTreeNode n2 (lf1 : c2)
  (TestTreeNode n1 c1) <> (TestTreeNode n2 c2) = TestTreeNode (n1 ++ " + " ++ n2) (c1 <> c2)

instance Show (TestTree a) where
  show t = printTree 0 t
    where
      printTree :: Int -> TestTree a -> String
      printTree _ TestTreeEmpty = ""
      printTree indent (TestTreeLeaf a) =
        printf
          "%s%s"
          (replicate indent ' ')
          (show a)
      printTree indent (TestTreeNode s tts) =
        printf
          "%s%s\n%s"
          (replicate indent ' ')
          s
          (L.intercalate "\n" (fmap (printTree (indent + 2)) (L.filter testTreeIsValid tts)))

      isTestTreeEmpty :: TestTree a -> Bool
      isTestTreeEmpty TestTreeEmpty = True
      isTestTreeEmpty _ = False

      testTreeIsValid :: TestTree a -> Bool
      testTreeIsValid TestTreeEmpty = False
      testTreeIsValid (TestTreeNode _ c) = not (all isTestTreeEmpty c) && not (null c) && any testTreeIsValid c
      testTreeIsValid _ = True

instance Eq TestCase where
  (TestCase nameL _ _) == (TestCase nameR _ _) = nameL == nameR

instance Show TestCase where
  show testCase =
    printf "%s" (testCaseName testCase)

data Details = Details {path :: [String], testResult :: TestResult}

instance Show Details where
  show :: Details -> String
  show = printDetails 0
    where
      printDetails :: Int -> Details -> String
      printDetails indent (Details [] testResult) =
        printf
          "%s%s\n\n"
          (replicate indent ' ')
          (showDetails testResult)
      printDetails indent (Details [_] testResult) = printDetails indent (Details [] testResult)
      printDetails indent (Details (p:ps) testResult) =
        printf
          "%s%s\n%s"
          (replicate indent ' ')
          p
          (printDetails (indent + 2) (Details ps testResult))

resultsToDetails :: TestResult -> Details
resultsToDetails (Passed tc) = Details [] (Passed tc)
resultsToDetails (Failed tc@(TestCase name _ _) ars) = Details [name] (Failed tc ars)
resultsToDetails (Todo tc@(TestCase name _ _) msg loc ars) = Details [name] (Todo tc msg loc ars)

testTreeResultsToDetails :: TestTree TestResult -> [Details]
testTreeResultsToDetails (TestTreeLeaf res) = [resultsToDetails res]
testTreeResultsToDetails (TestTreeNode name []) = []
testTreeResultsToDetails (TestTreeNode name (t:ts)) =
  let
    addContextToDetails :: String -> Details -> Details
    addContextToDetails context (Details p2 d2) = Details (context:p2) d2

    hasDetails :: Details -> Bool
    hasDetails (Details _ (Passed _)) = False
    hasDetails _ = True

    d = filter hasDetails $ fmap (addContextToDetails name) (testTreeResultsToDetails t)
    ds = filter hasDetails $ testTreeResultsToDetails (TestTreeNode name ts)
  in
    d ++ ds
testTreeResultsToDetails TestTreeEmpty = []

printDetailsList :: [Details] -> String
printDetailsList [] = ""
printDetailsList (d:ds) = printf "%s%s" (show d) (L.intercalate "\n" [printDetailsList ds])

data ResultTally = ResultTally { passed :: Int, failed :: Int, todo :: Int }

instance Monoid ResultTally where
  mempty = ResultTally 0 0 0

instance Semigroup ResultTally where
  ResultTally p1 f1 t1 <> ResultTally p2 f2 t2 = ResultTally (p1+p2) (f1+f2) (t1+t2)

instance Show ResultTally where
  show (ResultTally p f t) = "Passed: " ++ show p ++ "\nFailed: " ++ show f ++ "\nTodo: " ++ show t

resultToTally :: TestResult -> ResultTally
resultToTally (Passed _) = ResultTally 1 0 0
resultToTally (Failed _ _) = ResultTally 0 1 0
resultToTally (Todo {}) = ResultTally 0 0 1