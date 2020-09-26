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

tests :: TestTree
tests = testGroup "Parser tests" [
    expressionTests,
    commandTests
  ]

-----------------------------------------------------------

expressionTests :: TestTree
expressionTests = testGroup "Expression tests" [
    testVariable,
    testCallOrAccessExpression,
    testLeftAssociativity,
    testPriority
  ]

testVariable :: TestTree
testVariable = testCase "Variable expression" $
    parseMaybe (expression nilAnnp) "variable" @?= (Just $ Variable () "variable")

testCallOrAccessExpression :: TestTree
testCallOrAccessExpression = testCase "Cal or array access expression" $
    parseMaybe (expression nilAnnp) "foo[z](bar)[x][y]()(qux, 5)" @?= (Just $
        (Call () (
            (Call () (
                ArrayAccess () (
                    ArrayAccess () (
                        Call () (
                            ArrayAccess () (
                                Variable () "foo"
                            ) (Variable () "z")
                        ) [Variable () "bar"]
                    ) (Variable () "x")
                ) (Variable () "y")
            ) [])
        ) [Variable () "qux", Number () 5])
    )

testLeftAssociativity :: TestTree
testLeftAssociativity = testCase "Left associativity of subtraction" $
    parseMaybe (expression nilAnnp) "5 - 4 - 3" @?= Just (Subtraction () (Subtraction () (Number () 5) (Number () 4)) (Number () 3))

testPriority :: TestTree
testPriority = testCase "Priority of operations" $
    parseMaybe (expression nilAnnp) "1 + 2 * 3" @?= Just (Addition () (Number () 1) (Multiplication () (Number () 2) (Number () 3)))

-----------------------------------------------------------

commandTests :: TestTree
commandTests = testGroup "Command tests" [
    testCallCommand
  ]

testCallCommand :: TestTree
testCallCommand = testCase "Call command" $
    parseMaybe (command nilAnnp) "call(4, fun(\"text\"), false)(variable);" @?= (Just $
        CCall ()
            (Call () (Variable () "call") [Number () 4, Call () (Variable () "fun") [String () "text"], Boolean () False])
            [Variable () "variable"]
    )
