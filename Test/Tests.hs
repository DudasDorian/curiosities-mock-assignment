module Test.Tests where

import Control.Monad
import Control.Monad.State (State, execState)
import Data.Maybe (listToMaybe, fromMaybe)
import qualified Main
import qualified Test.Data as Data
import Test.SimpleTest
import Test.SimpleTest.Expectation
import Test.SimpleTest.Mock (makeMockIOState)
import qualified Test.SimpleTest.Mock as Mock
import Test.SimpleTest.TestCase
import Test.Util
import qualified Test.ArgsParser as P
import qualified Data.List as L
import Test.SimpleTest.Diff (Diff)

import Misc.ListOperations
import Math.BasicOperations
import Math.BaseTranslations
import Misc.FileOperations
import Result

data Options =
    DetailedFlag |
    FilterNameFlag |
    GroupNameFlag
    deriving (Ord, Eq, Show)

argd :: [ P.Arg Options ]
argd = [ P.Arg { P.argIndex = DetailedFlag,
               P.argName = Just "detailed",
               P.argAbbr = Just 'd',
               P.argType = Nothing },
         P.Arg { P.argIndex = FilterNameFlag,
               P.argName = Just "name",
               P.argAbbr = Just 'n',
               P.argType = Just (P.ArgtypeString Nothing) },
         P.Arg { P.argIndex = GroupNameFlag,
               P.argName = Nothing,
               P.argAbbr = Nothing,
               P.argType = Just (P.ArgtypeString Nothing) }]

tests =
  [ ("list", listTests),
    ("math", mathTests),
    ("io", ioTests)
  ]

main :: IO ()
main = do
  args <- P.parseArgsIO argd
  let detailed = P.gotArg args DetailedFlag
  let filter = P.getArg args FilterNameFlag
  let testGroup = fromMaybe testSuite (lookup (fromMaybe "" (P.getArg args GroupNameFlag)) tests)
  evalTestGroup detailed (filterByName filter testGroup)

testSuite :: TestTree TestCase
testSuite =
  group
    "test suite"
    (map snd tests)

listTests :: TestTree TestCase
listTests =
  group
    "list tests"
    [ group
        "finding common elements"
        (let
            wordsEn = ["fault", "aroma", "outgo", "trust", "tough", "bland", "axiom"]
            wordsRo = ["verde", "frant", "aroma", "fault", "bland"]
            emptyList = [] :: [String]
            intList1 = [1, 0, 2, 1] :: [Int]
            intList2 = [2, 1, 0] :: [Int]
            isSorted xs = all (<=0) $ zipWith (-) xs (tail xs)
            hasSameLengthAs xs ys = length xs == length ys
          in
          [
          testCase "sorting tests" [
              (null `shouldHold` findCommonElements emptyList emptyList, "handle the empty list", 5),
              (isSorted `shouldHold` findCommonElements intList2 intList2, "the list must be sorted", 10),
              (hasSameLengthAs intList1 `shouldHold` findCommonElements intList1 intList2, "if the input contains duplicates, they should not be removed", 5)
            ],
          testCase "element tests" [
              (findCommonElements emptyList wordsEn `shouldBe` [], "handle the empty list", 5),
              (findCommonElements wordsEn wordsEn `shouldBe` L.sort wordsEn, "if the input receives the same list, it should be returned", 5),
              (findCommonElements wordsEn wordsRo `shouldBe` ["aroma","bland","fault"], "you need to find the elements in common", 20)
            ]
        ])
    ]

mathTests =
  group
    "math tests"
    [ group
        "mathematical operations"
        [
          testCase "tetration tests" [
              ((2 #^^ 4) `shouldBe` 0, "handle case outside permitted range", 5),
              ((2 #^^ 3) `shouldBe` 16, "look at the formula for tetration", 10),
              ((2 ^ 2 #^^ 2 :: Integer) `shouldBe` 16, "tetration takes precedence over exponentiation", 10)
            ],
          testCase "operations tests" [
              (populateOperations 0 0 `shouldBe` Operations 0 1 1 0, "edge cases should be taken into consideration", 10),
              (populateOperations 4 3 `shouldBe`
                  Operations
                    7
                    12
                    64
                    13407807929942597099574024998205846127479365820592393377723561443721764030073546976801874298166903427690031858186486050853753882811946569946433649006084096
                  , "operation results should have the correct values", 10)
            ]
        ],
        group
        "base translations"
        [
          testCase "translating from base 4 to base 10" [
              (translateFrom4 "" `shouldBe` "0", "handle the empty string input", 5),
              (translateFrom4 "123" `shouldBe` "27", "123 in base 4 means 4^2*1 + 4^1*2 + 4^0*3 = 16 + 8 + 3, which in base 10 is 27", 10),
              (translateFrom4 "1asdf0" `shouldBe` "4", "ignore invalid characters", 10)
            ],
          testCase "translating from base x to base y" [
              (translateFromInto 2 10 "" `shouldBe` "0", "handle the empty string input", 5),
              (translateFromInto 2 2 "101" `shouldBe` "101", "translating from and into the same base should return the initial input", 10),
              (translateFromInto 2 10 "101" `shouldBe` "5", "101 in base 2 should return 5 in base 10", 10),
              (translateFromInto 2 10 "1asdf0" `shouldBe` "2", "ignore invalid characters", 10)
            ]
        ]
    ]

ioTests :: TestTree TestCase
ioTests =
  group
    "io tests"
    [
      testCase "reads from file" [
          (shouldReadFromFile "base4to10.txt" testFs (readBaseTranslationFile (Just "base4to10.txt")), "use the readFile function", 10)
        ],
      testCase "reads contents of file" [
          (shouldHaveResult testFs (readBaseTranslationFile (Just "missingfile.txt")) (Error FileNotFound), "use the doesFileExist function; when the file cannot be found return Error FileNotFound", 5),
          (shouldHaveResult testFsCorrupedFile (readBaseTranslationFile (Just "bad_base4to10.txt")) (Error ParseFailed), "when the file contents are corrupted return Error ParseFailed", 5),
          (shouldHaveResult testFs (readBaseTranslationFile (Just "base4to10.txt")) (Success (4, 10, "123")), "you should parse and return the contents of the file", 10)
        ]
    ]

testFs =
  makeMockIOState
    [
      ("base4to10.txt", "4 10 123")
    ]

testFsCorrupedFile =
  makeMockIOState
    [
      ("bad_base4to10.txt", "invalid")
    ]
