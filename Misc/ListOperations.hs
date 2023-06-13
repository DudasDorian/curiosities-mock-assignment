module Misc.ListOperations where
import Data.List
import Test.SimpleTest.Diff (Diff)

findCommonElements :: (Show a, Eq a, Diff a, Ord a) => [a] -> [a] -> [a]
findCommonElements _ _ = error "implement findCommonElements"