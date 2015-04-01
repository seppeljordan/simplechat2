module TestUtils where

import Control.Monad
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit

assertTrue = assertBool
assertFalse msg = assertTrue msg.not

runTest tests = do result <- runTestTT tests
                   when (errors result > 0) exitFailure
                   when (failures result > 0) exitFailure
                   exitSuccess
