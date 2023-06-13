{-# LANGUAGE GADTs #-}

module Test.SimpleTest where

import Control.Exception
import Data.Fixed (Centi, Fixed (MkFixed))
import qualified Data.List as L
import Test.SimpleTest.Expectation
import Test.SimpleTest.TestCase
import Text.Printf (printf)
import Control.Monad.Writer

group :: (Show a) => String -> [TestTree a] -> TestTree a
group name items = TestTreeNode {testGroupName = name, testGroupChildren = items}

testCase :: Expectation e => String -> [(e, String, Int)] -> TestTree TestCase
testCase name assertions =
  TestTreeLeaf TestCase {
    testCaseName = name,
    testCaseAssertions = mkAssertions assertions,
    enabled = True
    }

mkAssertions :: Expectation e => [(e, String, Int)] -> [Assertion]
mkAssertions [] = []
mkAssertions ((expectation, feedback, points):as) =
  Assertion {
    assertionExpectation = expectation,
    assertionFeedback = feedback,
    assertionPoints = MkFixed (fromIntegral points)
  } : mkAssertions as

verify :: [Assertion] -> IO [AssertionResult]
verify [] = return []
verify (a@(Assertion{assertionExpectation = e, assertionFeedback = _, assertionPoints = _}):as) = do
  res <- try $ evaluate $ holds e
  ress <- verify as
  case res of
    Left (ErrorCallWithLocation message location) -> return (TodoAssertion a message location : ress)
    Right res ->
      if res
        then return (PassedAssertion a : ress)
        else return (FailedAssertion a : ress)

toResult :: TestCase -> [AssertionResult] -> TestResult
toResult tc as =
    if anyFailed as
      then if allTodo as
        then Todo tc message location as
        else Failed tc as
    else Passed tc
  where
    anyFailed :: [AssertionResult] -> Bool
    anyFailed [] = False
    anyFailed (a:as) =
      case a of
        FailedAssertion _ -> True
        TodoAssertion {} -> True
        PassedAssertion _ -> anyFailed as

    allTodo :: [AssertionResult] -> Bool
    allTodo [] = False
    allTodo (a:as) =
      case a of
        TodoAssertion {} ->
          case as of
            [] -> True
            _ -> allTodo as
        _ -> False

    getTodoInfo :: [AssertionResult] -> (String, String)
    getTodoInfo (a:as) =
      case a of
        TodoAssertion {todoAssertionMessage = message, todoAssertionLocation = location} -> (message, location)
        _ -> getTodoInfo as
    getTodoInfo _ = mempty

    (message, location) = getTodoInfo as

runTest :: TestCase -> IO TestResult
runTest tc@(TestCase _ assertions _ ) = do
  ares <- verify assertions
  return $ toResult tc ares

runTestGroup :: TestTree TestCase -> IO (TestTree TestResult)
runTestGroup (TestTreeLeaf tc@(TestCase _  _ True)) = TestTreeLeaf <$> (runTest tc)
runTestGroup (TestTreeNode name tts) = TestTreeNode name <$> sequenceA (fmap (runTestGroup) tts)
runTestGroup _ = return mempty

runAndSeparateTestGroup :: Bool -> TestTree TestCase -> Writer (IO [Details]) (IO (TestTree TestResult))
runAndSeparateTestGroup showDetails (TestTreeLeaf tc) =
  let
    results = TestTreeLeaf <$> runTest tc
    details = if showDetails then testTreeResultsToDetails <$> results else mempty
  in
  do
    tell details
    return results
runAndSeparateTestGroup showDetails (TestTreeNode name tts) =
  let
    results = TestTreeNode name <$> traverse runTestGroup tts
    details = if showDetails then testTreeResultsToDetails <$> results else mempty
  in
  do
    tell details
    return results
runAndSeparateTestGroup _ _ = return mempty

calculateScoreAndTallyResults :: TestTree TestResult -> Writer ResultTally (TestPoints Centi)
calculateScoreAndTallyResults (TestTreeLeaf result) = do
  tell $ resultToTally result
  return $ resultToPoints result
calculateScoreAndTallyResults (TestTreeNode _ []) = return mempty
calculateScoreAndTallyResults (TestTreeNode s (t:ts)) = do
  let (tp, tl) = runWriter $ calculateScoreAndTallyResults t
  let (tps, tls) = runWriter $ calculateScoreAndTallyResults (TestTreeNode s ts)
  tell $ tl <> tls
  return $ tp <> tps
calculateScoreAndTallyResults _ = return mempty

disableTestCaseByName :: String -> TestCase -> TestCase
disableTestCaseByName filter (TestCase name expectation _)
  | filter `L.isInfixOf` name = TestCase name expectation True
  | otherwise = TestCase name expectation False

filterByName :: Maybe String -> TestTree TestCase -> TestTree TestCase
filterByName Nothing tt = tt
filterByName (Just filter) (TestTreeLeaf tc) = TestTreeLeaf $ disableTestCaseByName filter tc
filterByName filter (TestTreeNode grName tts) = TestTreeNode grName (fmap (filterByName filter) tts)
filterByName _ _ = TestTreeEmpty

disablePassedTestCase :: TestResult -> TestCase
disablePassedTestCase (Passed (TestCase name expectation _)) = TestCase name expectation False
disablePassedTestCase (Failed tc _) = tc
disablePassedTestCase (Todo tc _ _ _) = tc

getFailedTests :: TestTree TestResult -> TestTree TestCase
getFailedTests (TestTreeLeaf tr) = TestTreeLeaf $ disablePassedTestCase tr
getFailedTests (TestTreeNode grName tts) = TestTreeNode grName (fmap getFailedTests tts)
getFailedTests _ = TestTreeEmpty

evalTestGroup :: Bool -> TestTree TestCase -> IO ()
evalTestGroup showDetails gr = do
  let (resultsIO, detailsIO) = runWriter $ runAndSeparateTestGroup showDetails gr
  results <- resultsIO
  print results

  details <- detailsIO
  let detailsHeader = if showDetails && not (null details)  then "\n\n ---- Details ---- \n\n" else ""
  printf "%s\n" detailsHeader
  printf $ printDetailsList details

  let (score, tally) = runWriter $ calculateScoreAndTallyResults results
  printf "Final score: %s\n\n%s\n" (show score) (show tally)

  if null details 
    then return mempty
    else
      do
        putStrLn "\nPress 'R' to rerun failed tests: "
        c <- head <$> getLine
        if c == 'R' || c == 'r'
          then 
            do
              let (resultsIO, detailsIO) = runWriter $ runAndSeparateTestGroup showDetails (getFailedTests results)
              results <- resultsIO
              print results

              details <- detailsIO
              let detailsHeader = if showDetails && not (null details)  then "\n\n ---- Details ---- \n\n" else ""
              printf "%s\n" detailsHeader
              printf $ printDetailsList details
          else
            return mempty