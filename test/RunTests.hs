module Main(main) where

import Control.Monad
import System.Exit
import Test.HUnit

import Tests

main :: IO ()
main =
  do c <- runTestTT Tests.tests
     when (errors c /= 0 || failures c /= 0)
       exitFailure
