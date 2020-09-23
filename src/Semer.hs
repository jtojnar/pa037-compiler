{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{-| Semantic analyser, mainly type-checks the programs
-}
module Semer (typeCheck) where

import Ast
import Control.Applicative ((<|>), optional, some, many)
import Control.Monad (when)
import Data.HashMap (Map)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Void
import Data.Either (isLeft, isRight)
import Helpers
import Printer
import qualified Data.HashMap as Map
import Text.Megaparsec (SourcePos)

default (Text, Int)

-- TYPESs

data Context = Context {
    contextBindings :: Map Identifier Type,
    contextResult :: Type,
    contextReturned :: Bool,
    contextFunctionName:: Text
}

contextLookupBinding :: Identifier -> Context -> Maybe Type
contextLookupBinding name = Map.lookup name . contextBindings

contextBindName :: Text -> Type -> Context -> Context
contextBindName name ty originalContext@(Context {contextBindings}) = originalContext { contextBindings = Map.insert name ty contextBindings }

data SemanticError an = SemanticError [an] Text deriving (Eq, Show)

typeCheck :: Program ann -> ([SemanticError ann], Program (ann, Type))
typeCheck program =
    let
        functions = fmap funType (Map.fromList program)
        checkedFunctions = map (typeCheckFunction functions) program
    in
        (concatMap fst checkedFunctions, map snd checkedFunctions)

typeCheckFunction :: Map Identifier Type -> (Identifier, FunctionDefinition ann) -> ([SemanticError ann], (Identifier, FunctionDefinition (ann, Type)))
typeCheckFunction functions (name, (args, result, Nothing)) = ([], (name, (args, result, Nothing)))
typeCheckFunction functions (name, (args, result, Just body)) = (bodyErrors, (name, (args, result, Just newBody)))
    where
        context = Context {
            contextBindings = functions `Map.union` Map.fromList args,
            contextResult = result,
            contextReturned = False,
            contextFunctionName = name
        }
        (bodyErrors, newBody, newContext) = typeCheckCommands context body

{-| Type-check body of a function
-}
typeCheckCommands :: Context -> Commands ann -> ([SemanticError ann], Commands (ann, Type), Context)
typeCheckCommands originalContext =
    foldl checkCommand ([], [], originalContext)
    where
        checkCommand (errors, commands, previousContext) command =
            let
                (newErrors, newCommand, newContext) = typeCheckCommand previousContext command
            in
                (errors ++ newErrors, commands ++ [newCommand], newContext)

{-| Type-check body of a function
-}
typeCheckCommand :: Context -> Command ann -> ([SemanticError ann], Command (ann, Type), Context)
typeCheckCommand context (Conditional ann branches melse) =
    let
        branchesChecks = map (\(condition, body) ->
            let
                (cErrors, condition') = typeOf context condition
                ctype = semType condition'
                mismatchErrors = if ctype /= TBool then [SemanticError [expressionAnn condition] ("Condition needs to be of type bool, " <> ppType ctype <> " given.")] else []
                (bodyErrors, commands', context') = typeCheckCommands context body
                errors = cErrors ++ mismatchErrors ++ bodyErrors
            in (errors, (condition', commands'), context')) branches
        branchesErrors = concatMap (\(errs, _commands, _ctx) -> errs) branchesChecks
        branches' = map (\(_errs, branch, _ctx) -> branch) branchesChecks

        (elseErrors, melse', _elseContext') = case melse of
            Just elseBody ->
                let
                    (errors, elseBody', context') = typeCheckCommands context elseBody
                in
                    (errors, Just elseBody', context')
            Nothing -> ([], Nothing, context)

        errors = branchesErrors ++ elseErrors
    in
        (errors, Conditional (ann, TNil) branches' melse', context) -- context unchanged
typeCheckCommand context (ForEach ann item collection body) =
    let
        (cErrors, collection') = typeOf context collection
        ctype = semType collection'
        -- TODO: check that ctype is a iterable
        (bodyErrors, body', context') = typeCheckCommands context body
        errors = cErrors ++ bodyErrors
    in
        (errors, ForEach (ann, TNil) item collection' body', context')
typeCheckCommand context (While ann condition body) =
    let
        (cErrors, condition') = typeOf context condition
        ctype = semType condition'
        mismatchErrors = if ctype /= TBool then [SemanticError [expressionAnn condition] ("Condition needs to be of type bool, " <> ppType ctype <> " given.")] else []
        (bodyErrors, body', context') = typeCheckCommands context body
        errors = cErrors ++ mismatchErrors ++ bodyErrors
    in
        (errors, While (ann, TNil) condition' body', context)
typeCheckCommand context (Return ann expr) =
    let
        expectedType = contextResult context
        (exprErrors, expr') = typeOf context expr
        texpr = semType expr'
        mismatchErrors = if texpr /= expectedType then [SemanticError [ann] ("Function was declared as returning type ‘" <> ppType expectedType <> "’ but the return statement returns ‘" <> ppType texpr <> "’.")] else []
        context' = context { contextReturned = True }
        errors = exprErrors ++ mismatchErrors
    in
        (errors, Return (ann, TNil) expr', context')
typeCheckCommand context (Declaration ann name ty mexpr) =
    let
        context' = contextBindName name ty context
    in
        case mexpr of
            Just expr ->
                let
                    (exprErrors, expr') = typeOf context expr
                    texpr = semType expr'
                    mismatchErrors = if texpr /= ty then [SemanticError [ann] ("Variable " <> name <> " was declared as ‘" <> ppType ty <> "’ but it was set to ‘" <> ppType texpr <> "’.")] else []
                    errors = exprErrors ++ mismatchErrors
                in
                    (errors, Declaration (ann, TNil) name ty (Just expr'), context')
            Nothing -> ([], Declaration (ann, TNil) name ty Nothing, context')
typeCheckCommand context (Assignment ann name expr) =
    let
        (scopeErrors, ty) =
            case contextLookupBinding name context of
                Just ty -> ([], ty)
                Nothing ->
                    ([SemanticError [ann] ("Variable ‘" <> name <> "’ not declared.")], TNil) -- TODO: use bottom
        (terrors, expr') = typeOf context expr
        texpr = semType expr'
        mismatchErrors = if texpr /= ty then [SemanticError [ann] ("Variable " <> name <> " was declared as ‘" <> ppType ty <> "’ but it was set to ‘" <> ppType texpr <> "’.")] else []
        errors = scopeErrors ++ terrors ++ mismatchErrors
    in
        (errors, Assignment (ann, TNil) name expr', context)
typeCheckCommand context call@(CCall ann name args) =
    let
        (errors, expr') = typeOf context (Call ann name args)
        Call _ann _name args' = expr'
    in
        (errors, CCall (ann, TNil) name args', context)

checkNumericBinOp :: (Type -> Expression (ann, Type) -> Expression (ann, Type) -> Expression (ann, Type)) -> Context -> Expression ann -> Expression ann -> ([SemanticError ann], Expression (ann, Type))
checkNumericBinOp mkNode context l r =
    let
        (lerrors, l') = typeOf context l
        tl = semType l'
        (rerrors, r') = typeOf context r
        tr = semType r'
        (mismatchErrors, expectedType) = checkExpression
            [ (not (isNumericType tl), ("Left value needs to be a numeric type.", [expressionAnn l]))
            , (not (isNumericType tr), ("Right value needs to be a numeric type.", [expressionAnn r]))
            , (tl /= tr, ("Types of values need to match.", [expressionAnn l, expressionAnn r]))
            ]
            tl
        errors = lerrors ++ rerrors ++ mismatchErrors
    in
        (errors, mkNode expectedType l' r')

checkOrdBinOp :: (Type -> Expression (ann, Type) -> Expression (ann, Type) -> Expression (ann, Type)) -> Context -> Expression ann -> Expression ann -> ([SemanticError ann], Expression (ann, Type))
checkOrdBinOp mkNode context l r =
    let
        (lerrors, l') = typeOf context l
        tl = semType l'
        (rerrors, r') = typeOf context r
        tr = semType r'
        (mismatchErrors, expectedType) = checkExpression
            [ (not (isOrdType tl), ("Left value needs to be an ordered type.", [expressionAnn l]))
            , (not (isOrdType tr), ("Right value needs to be an ordered type.", [expressionAnn r]))
            , (tl /= tr, ("Types of values need to match.", [expressionAnn l, expressionAnn r]))
            ]
            TBool
        errors = lerrors ++ rerrors ++ mismatchErrors
    in
        (errors, mkNode expectedType l' r')

checkEqBinOp :: (Type -> Expression (ann, Type) -> Expression (ann, Type) -> Expression (ann, Type)) -> Context -> Expression ann -> Expression ann -> ([SemanticError ann], Expression (ann, Type))
checkEqBinOp mkNode context l r =
    let
        (lerrors, l') = typeOf context l
        tl = semType l'
        (rerrors, r') = typeOf context r
        tr = semType r'
        (mismatchErrors, expectedType) = checkExpression
            [ (not (isEqType tl), ("Left value needs to be a type with equivalence defined.", [expressionAnn l]))
            , (not (isEqType tr), ("Right value needs to be a type with equivalence defined.", [expressionAnn r]))
            , (tl /= tr, ("Types of values need to match.", [expressionAnn l, expressionAnn r]))
            ]
            TBool
        errors = lerrors ++ rerrors ++ mismatchErrors
    in
        (errors, mkNode expectedType l' r')

checkBoolBinOp :: (Type -> Expression (ann, Type) -> Expression (ann, Type) -> Expression (ann, Type)) -> Context -> Expression ann -> Expression ann -> ([SemanticError ann], Expression (ann, Type))
checkBoolBinOp mkNode context l r =
    let
        (lerrors, l') = typeOf context l
        tl = semType l'
        (rerrors, r') = typeOf context r
        tr = semType r'
        (mismatchErrors, expectedType) = checkExpression
            [ (not (isBooleanType tl), ("Left value needs to be a boolean.", [expressionAnn l]))
            , (not (isBooleanType tr), ("Right value needs to be a boolean.", [expressionAnn r]))
            ]
            TBool
        errors = lerrors ++ rerrors ++ mismatchErrors
    in
        (errors, mkNode expectedType l' r')

checkExpression :: [(Bool, (Text, [ann]))] -> Type -> ([SemanticError ann], Type)
checkExpression errors ty =
    let
        applicableErrors = map snd (filter fst errors)
    in
        case applicableErrors of
            [] -> ([], ty)
            _ -> (map (uncurry (flip SemanticError)) applicableErrors, TNil) -- TODO: use bottom type

typeOf :: Context -> Expression ann -> ([SemanticError ann], Expression (ann, Type))
typeOf context (Addition ann l r) = checkNumericBinOp (\ty -> Addition (ann, ty)) context l r
typeOf context (Subtraction ann l r) = checkNumericBinOp (\ty -> Subtraction (ann, ty)) context l r
typeOf context (Multiplication ann l r) = checkNumericBinOp (\ty -> Multiplication (ann, ty)) context l r
typeOf context (Division ann l r) = checkNumericBinOp (\ty -> Division (ann, ty)) context l r
typeOf context (Conjunction ann l r) = checkBoolBinOp (\ty -> Conjunction (ann, ty)) context l r
typeOf context (Disjunction ann l r) = checkBoolBinOp (\ty -> Disjunction (ann, ty)) context l r
typeOf context (Equality ann l r) = checkEqBinOp (\ty -> Equality (ann, ty)) context l r
typeOf context (Inequality ann l r) = checkEqBinOp (\ty -> Inequality (ann, ty)) context l r
typeOf context (LessThan ann l r) = checkOrdBinOp (\ty -> LessThan (ann, ty)) context l r
typeOf context (LessThanEqual ann l r) = checkOrdBinOp (\ty -> LessThanEqual (ann, ty)) context l r
typeOf context (Greater ann l r) = checkOrdBinOp (\ty -> Greater (ann, ty)) context l r
typeOf context (GreaterThanEqual ann l r) = checkOrdBinOp (\ty -> GreaterThanEqual (ann, ty)) context l r
typeOf context (Number ann val) = ([], Number (ann, TInt32) val)
typeOf context (Boolean ann val) = ([], Boolean (ann, TBool) val)
typeOf context (Character ann val) = ([], Character (ann, TChar) val)
typeOf context (String ann val) = ([], String (ann, TPtr TChar) val)
typeOf context (Variable ann name) =
    case contextLookupBinding name context of
        Just ty -> ([], Variable (ann, ty) name)
        Nothing ->
            ([SemanticError [ann] ("Variable " <> name <> " not defined.")],
            Variable (ann, TNil) name) -- TODO: use bottom
typeOf context (Call ann name args) =
    let
        (scopeErrors, fn) =
            case contextLookupBinding name context of
                Just ty -> ([], ty)
                Nothing ->
                    ([SemanticError [ann] ("Function ‘" <> name <> "’ not defined.")], TNil) -- TODO: use bottom
        (argumentErrors, resultType, args') =
            case fn of
                Function fnargs result ->
                    let
                        argumentCountErrors = if length fnargs /= length args then [SemanticError [ann] ("Function ‘" <> "’ expects " <> tshow (length fnargs) <> " arguments but " <> tshow (length args) <> " were given.")] else []
                        argChecks =
                            zipWith3
                                (\expected arg n ->
                                    let
                                        (argErrors, arg') = typeOf context arg
                                        actual = semType arg'
                                        mismatchErrors = if expected == actual then [] else [SemanticError [ann] ("Argument " <> tshow n <> " of function ‘" <> name <> "’ requires a value of type ‘" <> ppType expected <> "’ but ‘" <> ppType actual <> "’ was received.")]
                                        errors = argErrors ++ mismatchErrors
                                    in
                                        (errors, arg')
                                )
                                fnargs
                                args
                                [1..]
                        argumentValueErrors = concatMap fst argChecks
                        fnargs' = map snd argChecks
                        errors = argumentCountErrors ++ argumentValueErrors
                    in
                        (errors, result, fnargs')
                _ -> ([SemanticError [ann] ("Name ‘" <> name <> "’ is not a function.")], TNil, []) -- TODO: use bottom, maybe always check argErrors
        errors = scopeErrors ++ argumentErrors
    in
        (errors, Call (ann, resultType) name args')

{-| Get type from semantically annotated expression node.
-}
semType :: Expression (ann, tyann) -> tyann
semType = snd . expressionAnn
