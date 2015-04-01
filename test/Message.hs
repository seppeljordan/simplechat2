module Main where

import Control.Monad
import Network.SimpleChat.Classes
import Network.SimpleChat.Message
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit

assertTrue = assertBool
assertFalse msg = assertTrue msg.not

test_quit const = TestCase (do assertTrue "/quit should be a quit command" (isQuit (const "/quit"))
                               assertFalse "blabla shoud not be a quit command" (isQuit (const "blabla"))
                           )

test_error const = TestCase ( do assertTrue "/blabla should be an error" (isError (const "/blabla"))
                                 assertFalse "blabla should not be an error" (isError (const "blabla"))
                            )

test_help const = TestCase (assertTrue
                            "/help should be a help command"
                            (isHelp (const "/help"))
                           )

test_who const = TestCase (assertTrue
                           "/who should be a who command"
                           (isWho (const "/who"))
                          )

test_rename const = TestCase ( do assertEqual "'/rename test' should yield the rename text 'test'"
                                              (Just "test") (getRenameText (const "/rename test"))
                                  assertTrue "'/rename' should be an error" (isError (const "/rename"))
                             )

main = do result <- runTestTT cmdTests
          when (errors result > 0) exitFailure
          when (failures result > 0) exitFailure
          exitSuccess
    where cmdTests = TestList ( ($ parseCmd) `map`
                                [ test_quit
                                , test_help
                                , test_who
                                , test_error
                                , test_rename
                                ]
                              )
