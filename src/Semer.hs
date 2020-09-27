{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{-| Semantic analyser, mainly type-checks the programs
-}
module Semer (typeCheck, typeCheckCommands, Context(..), SemanticError(..)) where

import Ast
import Control.Applicative ((<|>), optional, some, many)
import Control.Monad (when)
import Data.HashMap (Map)
import Data.Maybe (isJust)
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
} deriving (Eq, Show)

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
typeCheckFunction functions (name, FunctionDefinition args result variadic Nothing) = ([], (name, FunctionDefinition args result variadic Nothing))
typeCheckFunction functions (name, FunctionDefinition args result variadic (Just body)) = (bodyErrors, (name, FunctionDefinition args result variadic (Just newBody)))
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
        checkCommand (previousErrors, commands, previousContext) command =
            let
                alreadyReturnedMessage = "Unexpected command, the function already returned."
                alreadyReturnedErrors = if contextReturned previousContext && not (any (\(SemanticError _ msg) -> msg == alreadyReturnedMessage) previousErrors) then [SemanticError [commandAnn command] alreadyReturnedMessage] else []
                (newErrors, newCommand, newContext) = typeCheckCommand previousContext command
            in
                (previousErrors ++ alreadyReturnedErrors ++ newErrors, commands ++ [newCommand], newContext)

{-| Type-check body of a function
-}
typeCheckCommand :: Context -> Command ann -> ([SemanticError ann], Command (ann, Type), Context)
typeCheckCommand context (Conditional ann branches melse) =
    let
        branchesChecks = map (\(condition, body) ->
            let
                (cErrors, condition') = typeOf context condition
                ctype = semType condition'
                mismatchErrors = if tEquals ctype TBool then [] else [SemanticError [expressionAnn condition] ("Condition needs to be of type bool, " <> ppType ctype <> " given.")]
                (bodyErrors, commands', context') = typeCheckCommands context body
                errors = cErrors ++ mismatchErrors ++ bodyErrors
            in (errors, (condition', commands'), context')) branches
        (branchesErrors, branches', branchesContexts) = unzip3 branchesChecks

        (elseErrors, melse', elseContext') = case melse of
            Just elseBody ->
                let
                    (errors, elseBody', context') = typeCheckCommands context elseBody
                in
                    (errors, Just elseBody', context')
            Nothing -> ([], Nothing, context)

        errors = concat branchesErrors ++ elseErrors

        -- Context remains mostly unchanged.
        -- But if all the branches returned (including the virtual else branch),
        -- we propagate the return status.
        -- Of course, we need to preserve the original return status if the block already returned previously.
        newContext = context {
            contextReturned = contextReturned context || (all contextReturned (branchesContexts ++ [elseContext']) && isJust melse)
        }
    in
        (errors, Conditional (ann, TNil) branches' melse', newContext)
typeCheckCommand context (ForEach ann item collection body) =
    let
        (cErrors, collection') = typeOf context collection
        ctype = semType collection'
        -- TODO: check that ctype is a iterable
        (bodyErrors, body', context') = typeCheckCommands context body
        errors = cErrors ++ bodyErrors
    in
        (errors, ForEach (ann, TNil) item collection' body', context) -- context unchanged
typeCheckCommand context (While ann condition body) =
    let
        (cErrors, condition') = typeOf context condition
        ctype = semType condition'
        mismatchErrors = if tEquals ctype TBool then [] else [SemanticError [expressionAnn condition] ("Condition needs to be of type bool, " <> ppType ctype <> " given.")]
        (bodyErrors, body', context') = typeCheckCommands context body
        errors = cErrors ++ mismatchErrors ++ bodyErrors
    in
        (errors, While (ann, TNil) condition' body', context)
typeCheckCommand context (Return ann expr) =
    let
        expectedType = contextResult context
        (exprErrors, expr') = typeOf context expr
        texpr = semType expr'
        mismatchErrors = if tEquals texpr expectedType then [] else [SemanticError [ann] ("Function was declared as returning type ‘" <> ppType expectedType <> "’ but the return statement returns ‘" <> ppType texpr <> "’.")]
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
                    mismatchErrors = if tEquals texpr ty then [] else [SemanticError [ann] ("Variable " <> name <> " was declared as ‘" <> ppType ty <> "’ but it was set to ‘" <> ppType texpr <> "’.")]
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
                    ([SemanticError [ann] ("Variable ‘" <> name <> "’ not declared.")], TBot)
        (terrors, expr') = typeOf context expr
        texpr = semType expr'
        mismatchErrors = if tEquals texpr ty then [] else [SemanticError [ann] ("Variable " <> name <> " was declared as ‘" <> ppType ty <> "’ but it was set to ‘" <> ppType texpr <> "’.")]
        errors = scopeErrors ++ terrors ++ mismatchErrors
    in
        (errors, Assignment (ann, TNil) name expr', context)
typeCheckCommand context call@(CCall ann callee args) =
    let
        (errors, expr') = typeOf context (Call ann callee args)
        Call _ann callee' args' = expr'
    in
        (errors, CCall (ann, TNil) callee' args', context)

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
            , (not (tEquals tl tr), ("Types of values need to match.", [expressionAnn l, expressionAnn r]))
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
            , (not (tEquals tl tr), ("Types of values need to match.", [expressionAnn l, expressionAnn r]))
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
            , (not (tEquals tl tr), ("Types of values need to match.", [expressionAnn l, expressionAnn r]))
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
            _ -> (map (uncurry (flip SemanticError)) applicableErrors, TBot)

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
            Variable (ann, TBot) name)
typeOf context (Call ann callee args) =
    let
        (parentErrrors, callee') = typeOf context callee
        (argumentErrors, resultType, args') =
            case semType callee' of
                Function fnargs result variadic ->
                    let
                        argCountOkay = if variadic then (>=) else (==)
                        atLeast = if variadic then "at least " else ""
                        argumentCountErrors = if not (argCountOkay (length args) (length fnargs)) then [SemanticError [ann] ("Function ‘" <> "’ expects " <> atLeast <> tshow (length fnargs) <> " arguments but " <> tshow (length args) <> " were given.")] else []
                        argChecks =
                            zipWith3
                                (\expected arg n ->
                                    let
                                        (argErrors, arg') = typeOf context arg
                                        actual = semType arg'
                                        mismatchErrors = if tEquals expected actual then [] else [SemanticError [ann] ("Argument " <> tshow n <> " of function ‘" <> ppExpr callee <> "’ requires a value of type ‘" <> ppType expected <> "’ but ‘" <> ppType actual <> "’ was received.")]
                                        errors = argErrors ++ mismatchErrors
                                    in
                                        (errors, arg')
                                )
                                (fnargs ++ repeat TBot) -- Variadic arguments are checked against TBot since we have no idea what their expected type is.
                                args
                                [1..]
                        argumentValueErrors = concatMap fst argChecks
                        fnargs' = map snd argChecks
                        errors = argumentCountErrors ++ argumentValueErrors
                    in
                        (errors, result, fnargs')
                _ ->
                    let
                        exprOrName =
                            case callee of
                                Variable _ _ -> "Name"
                                _ -> "Expression"
                    in
                        ([SemanticError [ann] (exprOrName <> " ‘" <> ppExpr callee <> "’ is not a function.")], TBot, []) -- TODO: maybe always check argErrors
        errors = parentErrrors ++ argumentErrors
    in
        (errors, Call (ann, resultType) callee' args')
typeOf context (ArrayAccess ann array index) =
    let
        (arrayErrors, array') = typeOf context array
        (indexErrrors, index') = typeOf context index

        indexTypeErrors = case semType index' of
            TInt32 -> []
            ty ->
                [SemanticError [ann] ("‘" <> ppExpr index <> "’ is not an integer so it cannot be an index.")]

        (accessErrors, resultType) = case semType array' of
            TPtr ty -> ([], ty)
            ty ->
                let
                    exprOrName =
                        case array of
                            Variable _ _ -> "Name"
                            _ -> "Expression"
                in
                    ([SemanticError [ann] (exprOrName <> " ‘" <> ppExpr array <> "’ is not a indexable.")], TBot)

        errors = arrayErrors ++ indexErrrors ++ indexTypeErrors ++ accessErrors
    in
        (errors, ArrayAccess (ann, resultType) array' index')

{-| Get type from semantically annotated expression node.
-}
semType :: Expression (ann, tyann) -> tyann
semType = snd . expressionAnn
