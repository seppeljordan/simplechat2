module Main where

import Network.SimpleChat.Classes
import Network.SimpleChat.Message
import Test.HUnit
import TestUtils

test_quit f = TestCase (do assertTrue "/quit should be a quit command" (isQuit (f "/quit"))
                           assertFalse "blabla shoud not be a quit command" (isQuit (f "blabla"))
                       )

test_error f = TestCase ( do assertTrue "/blabla should be an error" (isError (f "/blabla"))
                             assertFalse "blabla should not be an error" (isError (f "blabla"))
                        )

test_help f = TestCase (assertTrue
                        "/help should be a help command"
                        (isHelp (f "/help"))
                       )

test_who f = TestCase (assertTrue
                       "/who should be a who command"
                       (isWho (f "/who"))
                      )

test_rename f = TestCase ( do assertEqual "'/rename test' should yield the rename text 'test'"
                                          (Just "test") (getRenameText (f "/rename test"))
                              assertTrue "'/rename' should be an error" (isError (f "/rename"))
                         )

test_cmd f = TestList ( ($ f) `map` [test_quit, test_help, test_who, test_error, test_rename] )

main = do runTest $ TestList [ cmdTests
                             , textMessageTests
                             ]
    where cmdTests = test_cmd parseCmd
          textMessageTests = test_cmd TextMessage
