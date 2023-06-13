module Math.BasicOperations where
import Text.Printf (printf)
import Test.SimpleTest.Diff

data Operations = Operations { summation :: Integer, multiplication :: Integer, exponentiation :: Integer, tetration :: Integer } deriving Eq

o1 = Operations 1 1 1 1
o2 = Operations 1 12 1 2

instance Show Operations where
  show (Operations summation multiplication exponentiation tetration) = 
    printf "summation: %s\nmultiplication: %s\nexponentiation: %s\ntetration: %s" 
      (show summation) (show multiplication) (show exponentiation) (show tetration)

instance Diff Operations where
  showDifference :: Operations -> Operations -> [Showing String]
  showDifference o1@(Operations{summation = s1, multiplication = m1, exponentiation = e1, tetration = t1})
                 o2@(Operations{summation = s2, multiplication = m2, exponentiation = e2, tetration = t2})
    | o1 == o2 = [Normal $ show o1]
    | otherwise = Normal "summation: " : showDifference s1 s2
               ++ [Normal "\nmultiplication: "] ++ showDifference m1 m2
               ++ [Normal "\nexponentiation: "] ++ showDifference e1 e2
               ++ [Normal "\ntetration: "] ++ showDifference t1 t2

-- | (#^^) represents the tetration operator, it is a succession of exponentiations
--
-- >>> 2 #^^ 3
-- 16
--
-- >>> 10 #^^ 10
-- 0
-- 
-- * Hint: use the correct infix definition
infix 0 #^^
(#^^) :: Integer -> Integer -> Integer
a #^^ n = error "implement tetration"

-- >>> populateOperations 2 3
-- summation: 5
-- multiplication: 6
-- exponentiation: 8
-- tetration: 16
populateOperations :: Integer -> Integer -> Operations
populateOperations _ _ = error "implement populateOperations"