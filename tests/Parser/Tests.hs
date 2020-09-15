{-# LANGUAGE OverloadedStrings #-}

module Parser.Tests (tests) where

import Ast
import Data.Text (Text)
import Parser
import Test.HUnit ((@?=))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Text.Megaparsec (parseMaybe)

default (Text)

nilAnnp = return ()

testLeftAssociativity :: TestTree
testLeftAssociativity = testCase "Left associativity of subtraction" $
    parseMaybe (expression nilAnnp) "5 - 4 - 3" @?= Just (Subtraction () (Subtraction () (Number () 5) (Number () 4)) (Number () 3))

testPriority :: TestTree
testPriority = testCase "Priority of operations" $
    parseMaybe (expression nilAnnp) "1 + 2 * 3" @?= Just (Addition () (Number () 1) (Multiplication () (Number () 2) (Number () 3)))

tests :: TestTree
tests = testGroup "Parser tests" [
    testLeftAssociativity,
    testPriority
  ]
