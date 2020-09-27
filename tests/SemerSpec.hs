{-# LANGUAGE OverloadedStrings #-}

module SemerSpec where

import Ast
import qualified Data.HashMap as Map
import Data.Text (Text)
import Semer
import Test.Hspec

default (Text)

emptyContext = Context {
    contextBindings = mempty,
    contextResult = TInt32,
    contextReturned = False,
    contextFunctionName = "foo"
}

spec :: Spec
spec = do
    describe "Semer" $ do
        describe "In command blocks" $ do
            describe "Context" $ do
                it "is not leaking" $
                    let
                        block =
                          [
                            Conditional () [
                                (Boolean () False, [Declaration () "a" TInt32 (Just (Number () 1))]),
                                (Boolean () True, [Declaration () "a" TInt32 (Just (Number () 2))])
                            ] (Just [Declaration () "a" TInt32 (Just (Number () 3))]),
                            While () (Boolean () False) [Declaration () "a" TInt32 (Just (Number () 4))],
                            Declaration () "b" TInt32 (Just (Number () 5)),
                            While () (Boolean () False) [Declaration () "b" TBool (Just (Boolean () False))]
                          ]
                        expectedContext = emptyContext { contextBindings = Map.fromList [("b", TInt32)] }

                        (errors, _commands, context') = typeCheckCommands emptyContext block
                    in do
                        context' `shouldBe` expectedContext
                        errors `shouldBe` []

            describe "Return" $ do
                it "should mark rest of the block unreachable" $
                    let
                        block =
                          [
                            Return () (Number () 1),
                            While () (Boolean () False) []
                          ]
                        expectedContext = emptyContext { contextReturned = True }

                        (errors, _commands, context') = typeCheckCommands emptyContext block
                    in do
                        context' `shouldBe` expectedContext
                        errors `shouldBe` [SemanticError [()] "Unexpected command, the function already returned."]

                describe "when in a single branch of nested block" $ do
                    it "should NOT mark rest of the block unreachable" $
                        let
                            block =
                              [
                                While () (Variable () "foo") [Return () (Number () 1)],
                                Conditional () [
                                    (Boolean () False, [Return () (Number () 1)])
                                ] Nothing
                              ]
                            baseContext = emptyContext { contextBindings = Map.fromList [("foo", TBool)] }

                            (errors, _commands, context') = typeCheckCommands baseContext block
                        in do
                            context' `shouldBe` baseContext
                            errors `shouldBe` []

                describe "when in all branches of nested block" $ do
                    it "should mark rest of the block unreachable" $
                        let
                            block =
                              [
                                Conditional () [
                                    (Boolean () False, [Return () (Number () 1)]),
                                    (Boolean () True, [Return () (Number () 2)])
                                ] (Just [Return () (Number () 3)])
                              ]
                            expectedContext = emptyContext { contextReturned = True }

                            (errors, _commands, context') = typeCheckCommands emptyContext block
                        in do
                            context' `shouldBe` expectedContext
                            errors `shouldBe` []

        describe "Function" $ do
            it "expects a return statement" $
                let
                    fn = FunctionDefinition () mempty TInt32 False (Just [])
                    functions = mempty
                    (errors, _fn') = typeCheckFunction functions ("foo", fn)
                in do
                    errors `shouldBe` [SemanticError [()] "Function “foo” does not contain a return statement in all branches."]
