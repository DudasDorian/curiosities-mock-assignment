{-# LANGUAGE DeriveAnyClass #-}
module Misc.FileOperations where
import Test.SimpleTest.Mock (TestableMonadIO (..))
import Result
import Test.SimpleTest.Diff
import Prelude hiding (readFile)

data LoadingError = FileNotFound | ParseFailed deriving (Eq, Show, Diff)

readBaseTranslationFile :: TestableMonadIO io => Maybe String -> io (Result LoadingError (Int, Int, String))
readBaseTranslationFile _ = error "implement readBaseTranslationFile"