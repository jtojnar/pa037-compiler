{-# LANGUAGE OverloadedStrings #-}

module ParserSpec where

import Ast
import Data.Text (Text)
import Parser
import Test.Hspec
import Text.Megaparsec (parseMaybe)

default (Text)

nilAnnp = return ()

spec :: Spec
spec = do
    describe "Parser" $ do
        describe "Expressions" $ do
            describe "Variable" $ do
                it "should be parsed as variable" $ do
                    parseMaybe (expression nilAnnp) "variable" `shouldBe` (Just $ Variable () "variable")

            describe "Call or array access expression" $ do
                it "should be parsed correctly" $ do
                    parseMaybe (expression nilAnnp) "foo[z](bar)[x][y]()(qux, 5)" `shouldBe`
                        (Just $
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

            describe "String literals" $ do
                it "should allow spaces around them" $ do
                    parseMaybe (expression nilAnnp) "( \"bar\"\t)" `shouldBe`
                        (Just $ String () "bar")

                it "should preserve spaces inside them" $ do
                    parseMaybe (expression nilAnnp) "\"\tbar \"" `shouldBe`
                        (Just $ String () "\tbar ")

            describe "Char literals" $ do
                it "should allow spaces around them" $ do
                    parseMaybe (expression nilAnnp) "( 'r'\t)" `shouldBe`
                        (Just $ Character () 'r')

                it "should only allow a single character except for escapes" $ do
                    parseMaybe (expression nilAnnp) "' x '" `shouldBe` Nothing
                    parseMaybe (expression nilAnnp) "'\\n'" `shouldBe` (Just $ Character () '\n')


            describe "Subtraction" $ do
                it "should associate from left" $ do
                    parseMaybe (expression nilAnnp) "5 - 4 - 3" `shouldBe` Just (Subtraction () (Subtraction () (Number () 5) (Number () 4)) (Number () 3))

            describe "Operations" $ do
                it "should respect priority" $
                    parseMaybe (expression nilAnnp) "1 + 2 * 3" `shouldBe` Just (Addition () (Number () 1) (Multiplication () (Number () 2) (Number () 3)))

        describe "Commands" $ do
            describe "Call command" $ do
                it "should be parsed correctly" $ do
                    parseMaybe (command nilAnnp) "call(4, fun(\"text\"), false)(variable);" `shouldBe`
                        (Just $
                            CCall ()
                                (Call () (Variable () "call") [Number () 4, Call () (Variable () "fun") [String () "text"], Boolean () False])
                                [Variable () "variable"]
                        )
