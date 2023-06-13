{-# LANGUAGE DeriveAnyClass #-}

module Test.SimpleTest.Diff where
import qualified Test.SimpleTest.Color as Color
import Text.Printf

data Showing a = Highlight a | Normal a deriving (Show, Eq, Diff)

-- | The 'Diff' class defines the showDifference function, highlighting the differences on the first argument compared to the second argument.
--
-- Can be derived by using the 'DeriveAnyClass' language pragma.
--
-- The default implementation uses the equality property defined in the 'Eq' instance and the String defined in the 'Show' instance.
--
class (Show a, Eq a) => Diff a where
    -- | Returns the string from the 'Show' instance of the first argument wrapped in the 'Showing' data type, highlighted if there is a difference to the second argument.
    -- 
    -- >>> showDifference 1 2
    -- [Highlight "1"]
    --
    -- >>> showDifference 1 1
    -- [Normal "1"]
    -- 
    -- 
    showDifference :: a -> a -> [Showing String]
    showDifference a b
        | a == b = [Normal (show a)]
        | otherwise = [Highlight (show a)]

-- Unit Type
deriving instance Diff ()

-- Lists
instance (Eq a, Show a) => Diff [a] where
    -- inspired by the implementation of showList:
    -- https://hackage.haskell.org/package/base-4.9.0.0/docs/src/GHC.Show.html#showList
    showDifference :: (Eq a, Show a) => [a] -> [a] -> [Showing String]
    showDifference xs@(_:_) [] = [Highlight $ show xs]
    showDifference [] _ = [Highlight "[]"]
    showDifference (x:xs) (y:ys) = Normal "[" : firstElement : diffHelper xs ys where
        firstElement :: Showing String
        firstElement
            | x == y = Normal $ show x
            | otherwise = Highlight $ show x

        diffHelper :: (Eq a, Show a) => [a] -> [a] -> [Showing String]
        diffHelper [] _ = [Normal "]"]
        diffHelper (x:xs) [] = Normal "," : Highlight (show x) : diffHelper xs []
        diffHelper (x:xs) (y:ys)
            | x == y = Normal "," : Normal (show x) : diffHelper xs ys
            | otherwise = Normal "," : Highlight (show x) : diffHelper xs ys

-- String
instance {-# OVERLAPS #-} Diff String where
    showDifference :: String -> String -> [Showing String]
    showDifference xs@(_:_) [] = [Highlight $ show xs]
    showDifference [] _ = [Highlight "\"\""]
    showDifference a b = Normal "\"" : diffHelper a b ++ [Normal "\""] where
        diffHelper :: String -> String -> [Showing String]
        diffHelper [] _ = []
        diffHelper xs [] = [Highlight xs]
        diffHelper (x:xs) (y:ys)
            | x == y = Normal [x] : diffHelper xs ys
            | otherwise = Highlight [x] : diffHelper xs ys

-- GHC.Types (ghc-prim)
deriving instance Diff Bool
deriving instance Diff Char
deriving instance Diff Int
deriving instance Diff Word
deriving instance Diff Float
deriving instance Diff Double
deriving instance Diff Ordering

-- GHC.Num (base)
deriving instance Diff Integer

-- GHC.Maybe (base)
instance (Show a, Eq a) => Diff (Maybe a) where
    showDifference :: (Show a, Eq a) => Maybe a -> Maybe a -> [Showing String]
    showDifference Nothing Nothing = [Normal "Nothing"]
    showDifference Nothing (Just _) = [Highlight "Nothing"]
    showDifference (Just a) Nothing = [Highlight $ show (Just a)]
    showDifference (Just a) (Just b)
        | a == b = [Normal $ show (Just a)]
        | otherwise = [Normal "Just ", Highlight $ show a]

-- Tuples
instance (Show a, Show b, Eq a, Eq b) => Diff (a, b) where
    showDifference :: (a, b) -> (a, b) -> [Showing String]
    showDifference (a1, b1) (a2, b2)
        | (a1, b1) == (a2, b2) = [Normal $ show (a1, b1)]
        | a1 == a2 = [Normal $ "(" ++ show a1 ++ ",", Highlight $ show b1, Normal ")"]
        | b1 == b2 = [Normal "(", Highlight $ show a1, Normal $ "," ++ show b1 ++ ")"]
        | otherwise = [Normal "(", Highlight $ show a1, Normal ",", Highlight $ show b1, Normal ")"]

instance (Show a, Show b, Show c, Eq a, Eq b, Eq c) => Diff (a, b, c) where
    showDifference :: (a, b, c) -> (a, b, c) -> [Showing String]
    showDifference (a1, b1, c1) (a2, b2, c2)
        | (a1, b1, c1) == (a2, b2, c2) = [Normal $ show (a1, b1, c2)]
        | a1 == a2 && b1 == b2 = [Normal $ "(" ++ show a1 ++ ",", Normal $ show b1 ++ ",", Highlight $ show c1, Normal ")"]
        | a1 == a2 && c1 == c2 = [Normal $ "(" ++ show a1 ++ ",", Highlight $ show b1, Normal $ "," ++ show c1 ++ ")"]
        | b1 == b2 && c1 == c2 = [Normal "(", Highlight $ show a1, Normal $ "," ++ show b1 ++ "," ++ show c1 ++ ")"]
        | a1 == a2 = [Normal $ "(" ++ show a1 ++ ",", Highlight $ show b1, Normal ",", Highlight $ show c1, Normal ")"]
        | b1 == b2 = [Normal "(" , Highlight $ show a1, Normal $ "," ++ show b1 ++ ",", Highlight $ show c1, Normal ")"]
        | c1 == c2 = [Normal "(", Highlight $ show a1, Normal ",", Highlight $ show b1, Normal ",", Normal $ show c1 ++ ")"]
        | otherwise = [Normal "(", Highlight $ show a1, Normal ",", Highlight $ show b1, Normal ",", Highlight $ show c1, Normal ")"]