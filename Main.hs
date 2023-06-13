module Main where

import Control.Applicative
import Data.List
import Data.Maybe
import qualified System.Environment as ENV
import System.IO
import Text.Printf
import Result

import Misc.FileOperations
import Math.BasicOperations
import Math.BaseTranslations

main :: IO ()
main = do
  args <- ENV.getArgs
  result <- readBaseTranslationFile (listToMaybe args)
  case result of
    Success (x, y, input) -> do 
                    printf "For inputs %d and %d:\n" x y
                    print $ populateOperations (toInteger x) (toInteger y)
                    printf "%s in base %d is %s in base %d:\n" input x (translateFromInto x y input) y
    Error err -> printf "Encountered an error while reading file: %s\n" (show err)