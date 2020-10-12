{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{-| Semantic analyser, mainly type-checks the programs
-}
module Semer (typeCheck, typeCheckCommands, typeCheckFunction, Context(..), SemanticError(..)) where

import Ast
import Data.HashMap (Map)
import Data.List (findIndex)
import Data.Maybe (isJust)
import Data.Text (Text)
import Helpers
import Printer
import qualified Data.HashMap as Map

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
typeCheckFunction _functions (name, FunctionDefinition endAnn args result variadic Nothing) = ([], (name, FunctionDefinition (endAnn, tnil) args result variadic Nothing)) -- extern function has nothing to check.
typeCheckFunction functions (name, FunctionDefinition endAnn args result variadic (Just body)) =
    let
        context = Context {
            contextBindings = functions `Map.union` Map.fromList args,
            contextResult = result,
            contextReturned = False,
            contextFunctionName = name
        }
        (bodyErrors, newBody, newContext) = typeCheckCommands context body
        missingReturnErrors = if contextReturned newContext then [] else [SemanticError [endAnn] ("Function “" <> name <> "” does not contain a return statement in all branches.")]
    in
        -- The type part of end annotation returned in FunctionDefinition is not used but it is nicer than parametrizing everything by a second annotation type.
        (bodyErrors ++ missingReturnErrors, (name, FunctionDefinition (endAnn, tnil) args result variadic (Just newBody)))

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
                branchErrors = cErrors ++ mismatchErrors ++ bodyErrors
            in (branchErrors, (condition', commands'), context')) branches
        (branchesErrors, branches', branchesContexts) = unzip3 branchesChecks

        (elseErrors, melse', elseContext') = case melse of
            Just elseBody ->
                let
                    (elseErrors', elseBody', context') = typeCheckCommands context elseBody
                in
                    (elseErrors', Just elseBody', context')
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
        (errors, Conditional (ann, tnil) branches' melse', newContext)
typeCheckCommand context (While ann condition body) =
    let
        (cErrors, condition') = typeOf context condition
        ctype = semType condition'
        mismatchErrors = if tEquals ctype TBool then [] else [SemanticError [expressionAnn condition] ("Condition needs to be of type bool, " <> ppType ctype <> " given.")]
        (bodyErrors, body', _context') = typeCheckCommands context body
        errors = cErrors ++ mismatchErrors ++ bodyErrors
    in
        (errors, While (ann, tnil) condition' body', context)
typeCheckCommand context (Return ann expr) =
    let
        expectedType = contextResult context
        (exprErrors, expr') = typeOf context expr
        texpr = semType expr'
        mismatchErrors = if tEquals texpr expectedType then [] else [SemanticError [ann] ("Function was declared as returning type ‘" <> ppType expectedType <> "’ but the return statement returns ‘" <> ppType texpr <> "’.")]
        context' = context { contextReturned = True }
        errors = exprErrors ++ mismatchErrors
    in
        (errors, Return (ann, tnil) expr', context')
typeCheckCommand context (Declaration ann name declaredType mexpr) =
    case mexpr of
        Just expr ->
            let
                (exprErrors, expr') = typeOf context expr
                texpr = semType expr'
                mismatchErrors = if tEquals texpr declaredType then [] else [SemanticError [ann] ("Variable " <> name <> " was declared as ‘" <> ppType declaredType <> "’ but it was set to ‘" <> ppType texpr <> "’.")]
                errors = exprErrors ++ mismatchErrors
                declaredType' = case declaredType of
                    TBot -> texpr -- Type omitted and inferred from value type
                    ty -> ty
                context' = contextBindName name declaredType' context
            in
                (errors, Declaration (ann, tnil) name declaredType' (Just expr'), context')
        Nothing ->
            let
                context' = contextBindName name declaredType context
            in
                ([], Declaration (ann, tnil) name declaredType Nothing, context')
typeCheckCommand context (Assignment ann lhs rhs) =
    let
        (scopeErrors, lhs') = typeOf context lhs
        tl = semType lhs'
        (terrors, rhs') = typeOf context rhs
        tr = semType rhs'
        varOrElse = case lhs' of
            Variable _ _ -> "Variable"
            _ -> "Lvalue"
        mismatchErrors = if tEquals tr tl then [] else [SemanticError [ann] (varOrElse <> " " <> ppExpr lhs <> " is declared as ‘" <> ppType tl <> "’ but it was set to ‘" <> ppType tr <> "’.")]
        errors = scopeErrors ++ terrors ++ mismatchErrors
    in
        (errors, Assignment (ann, tnil) lhs' rhs', context)
typeCheckCommand context (CCall ann callee args) =
    let
        (errors, rhs') = typeOf context (Call ann callee args)
        Call _ann callee' args' = rhs'
    in
        (errors, CCall (ann, tnil) callee' args', context)

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
typeOf context (Negation ann inner) =
    let
        (innerErrors, inner') = typeOf context inner
        innerType = semType inner'
        (mismatchErrors, expectedType) = checkExpression
            [ (not (isBooleanType innerType), ("Negated value needs to be a boolean.", [expressionAnn inner]))
            ]
            TBool
        errors = innerErrors ++ mismatchErrors
    in
        (errors, Negation (ann, expectedType) inner')
typeOf context (Equality ann l r) = checkEqBinOp (\ty -> Equality (ann, ty)) context l r
typeOf context (Inequality ann l r) = checkEqBinOp (\ty -> Inequality (ann, ty)) context l r
typeOf context (LessThan ann l r) = checkOrdBinOp (\ty -> LessThan (ann, ty)) context l r
typeOf context (LessThanEqual ann l r) = checkOrdBinOp (\ty -> LessThanEqual (ann, ty)) context l r
typeOf context (Greater ann l r) = checkOrdBinOp (\ty -> Greater (ann, ty)) context l r
typeOf context (GreaterThanEqual ann l r) = checkOrdBinOp (\ty -> GreaterThanEqual (ann, ty)) context l r
typeOf _context (Number ann val) = ([], Number (ann, TInt32) val)
typeOf _context (Boolean ann val) = ([], Boolean (ann, TBool) val)
typeOf _context (Character ann val) = ([], Character (ann, TChar) val)
typeOf _context (String ann val) = ([], String (ann, TPtr TChar) val)
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
                                        singleArgErrors = argErrors ++ mismatchErrors
                                    in
                                        (singleArgErrors, arg')
                                )
                                (fnargs ++ repeat TBot) -- Variadic arguments are checked against TBot since we have no idea what their expected type is.
                                args
                                [ 1 :: Int .. ]
                        argumentValueErrors = concatMap fst argChecks
                        fnargs' = map snd argChecks
                        argumentsErrors = argumentCountErrors ++ argumentValueErrors
                    in
                        (argumentsErrors, result, fnargs')
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
            TChar -> []
            _ty ->
                [SemanticError [ann] ("‘" <> ppExpr index <> "’ is not an integer so it cannot be an index.")]

        (accessErrors, resultType) = case semType array' of
            TPtr ty -> ([], ty)
            TArray _size ty -> ([], ty)
            _ty ->
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
typeOf context (ProductFieldAccess ann prod name) =
    let
        (prodErrors, prod') = typeOf context prod
        (accessErrors, resultIndex, resultType) = case semType prod' of
            TProduct fields ->
                case findIndex ((== name) . fst) fields of
                    Just index -> ([], tshow index, snd (fields !! index))
                    Nothing -> ([SemanticError [ann] ("Missing field ‘" <> name <> "’.")], "-1", TBot)
            _ty ->
                let
                    exprOrName =
                        case prod of
                            Variable _ _ -> "Name"
                            _ -> "Expression"
                in
                    ([SemanticError [ann] (exprOrName <> " ‘" <> ppExpr prod <> "’ is not a product.")], "-1", TBot)

        errors = prodErrors ++ accessErrors
    in
        (errors, ProductFieldAccess (ann, resultType) prod' resultIndex) -- TODO: pass index using better method
typeOf context (AddressOf ann inner) =
    let
        (innerErrors, inner') = typeOf context inner
        innerType = semType inner'

        icompatibleInnerErrors = case inner of
            ArrayAccess _ _ _ -> []
            Variable _ _ -> []
            _ty ->
                [SemanticError [ann] "You can only get memory addresses for variables and array items."]

        errors = innerErrors ++ icompatibleInnerErrors
    in
        (errors, AddressOf (ann, TPtr innerType) inner')

{-| Get type from semantically annotated expression node.
-}
semType :: Expression (ann, tyann) -> tyann
semType = snd . expressionAnn
