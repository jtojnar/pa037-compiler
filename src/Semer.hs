{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{-| Semantic analyser, mainly type-checks the programs
-}
module Semer (typeCheck) where

import Ast
import Control.Applicative ((<|>), optional, some, many)
import Control.Monad (when, void)
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

joinFunctorList :: [Either [SemanticError ann] ()] -> Either [SemanticError ann] ()
joinFunctorList = foldl accum (Right ())
  where
    accum (Left panns) (Left aanns) = Left (panns ++ aanns)
    accum (Left panns) (Right ()) = Left panns
    accum (Right ()) (Left aanns) = Left aanns
    accum (Right ()) (Right ()) = Right ()

typeCheck :: Program ann -> Either [SemanticError ann] ()
typeCheck program =
    joinFunctorList (map (typeCheckFunction functions) program)
    where
        functions = fmap funType (Map.fromList program)

typeCheckFunction :: Map Identifier Type -> (Identifier, FunctionDefinition ann) -> Either [SemanticError ann] ()
typeCheckFunction functions (name, (args, result, body)) = void (typeCheckCommands context body)
    where
        context = Context {
            contextBindings = functions `Map.union` Map.fromList args,
            contextResult = result,
            contextReturned = False,
            contextFunctionName = name
        }

{-| Type-check body of a function
-}
typeCheckCommands :: Context -> Commands ann -> Either [SemanticError ann] Context
typeCheckCommands originalContext = foldl checkCommand (Right originalContext)
    where
        checkCommand epreviousContext command = do
            previousContext <- epreviousContext
            typeCheckCommand previousContext command

{-| Type-check body of a function
-}
typeCheckCommand :: Context -> Command ann -> Either [SemanticError ann] Context
typeCheckCommand context (Conditional ann branches melse) = branchesCheck *> elseBodyCheck
    where
        branchesCheck = mapM_ (\(condition, body) -> do
            ctype <- typeOf context condition
            when (ctype /= TBool) (Left [SemanticError [expressionAnn condition] ("Condition needs to be of type bool, " <> ppType ctype <> " given.")])
            typeCheckCommands context body) branches
        elseBodyCheck = case melse of
            Just elseBody -> typeCheckCommands context elseBody
            Nothing -> pure context
typeCheckCommand context (ForEach ann item collection body) = typeCheckCommands context body *> pure context
typeCheckCommand context (While ann expr body) = typeCheckCommands context body *> pure context
typeCheckCommand context (Return ann expr) =
    exprCheck *> Right (context { contextReturned = True })
  where
    exprCheck = do
        let expected = contextResult context
        texpr <- typeOf context expr
        when (texpr /= expected) (Left [SemanticError [ann] ("Function was declared as returning type ‘" <> ppType expected <> "’ but the return statement returns ‘" <> ppType texpr <> "’.")])
typeCheckCommand context (Declaration ann name ty mexpr) =
    exprCheck *> pure (contextBindName name ty context)
  where
    exprCheck = case mexpr of
        Just expr -> do
            texpr <- typeOf context expr
            when (texpr /= ty) (Left [SemanticError [ann] ("Variable " <> name <> " was declared as ‘" <> ppType ty <> "’ but it was set to ‘" <> ppType texpr <> "’.")])
        Nothing -> return ()
typeCheckCommand context (Assignment ann name expr) = do
    ty <- maybeToEither [SemanticError [ann] ("Variable ‘" <> name <> "’ not declared.")] (contextLookupBinding name context)
    exprCheck ty *> pure context
  where
    exprCheck ty = do
        texpr <- typeOf context expr
        when (texpr /= ty) (Left [SemanticError [ann] ("Variable " <> name <> " was declared as ‘" <> ppType ty <> "’ but it was set to ‘" <> ppType texpr <> "’.")])
typeCheckCommand context (CCall ann name args) =
    typeOf context (Call ann name args) >> pure context

checkNumericBinOp :: Context -> Expression ann -> Expression ann -> Either [SemanticError ann] Type
checkNumericBinOp context l r = do
    tl <- typeOf context l
    tr <- typeOf context r
    checkExpression
        [ (not (isNumericType tl), ("Left value needs to be a numeric type.", [expressionAnn l]))
        , (not (isNumericType tr), ("Right value needs to be a numeric type.", [expressionAnn r]))
        , (tl /= tr, ("Types of values need to match.", [expressionAnn l, expressionAnn r]))
        ]
        tl

checkOrdBinOp :: Context -> Expression ann -> Expression ann -> Either [SemanticError ann] Type
checkOrdBinOp context l r = do
    tl <- typeOf context l
    tr <- typeOf context r
    checkExpression
        [ (not (isOrdType tl), ("Left value needs to be an ordered type.", [expressionAnn l]))
        , (not (isOrdType tr), ("Right value needs to be an ordered type.", [expressionAnn r]))
        , (tl /= tr, ("Types of values need to match.", [expressionAnn l, expressionAnn r]))
        ]
        TBool

checkEqBinOp :: Context -> Expression ann -> Expression ann -> Either [SemanticError ann] Type
checkEqBinOp context l r = do
    tl <- typeOf context l
    tr <- typeOf context r
    checkExpression
        [ (not (isEqType tl), ("Left value needs to be a type with equivalence defined.", [expressionAnn l]))
        , (not (isEqType tr), ("Right value needs to be a type with equivalence defined.", [expressionAnn r]))
        , (tl /= tr, ("Types of values need to match.", [expressionAnn l, expressionAnn r]))
        ]
        TBool

checkBoolBinOp :: Context -> Expression ann -> Expression ann -> Either [SemanticError ann] Type
checkBoolBinOp context l r = do
    tl <- typeOf context l
    tr <- typeOf context r
    checkExpression
        [ (not (isBooleanType tl), ("Left value needs to be a boolean.", [expressionAnn l]))
        , (not (isBooleanType tr), ("Right value needs to be a boolean.", [expressionAnn r]))
        ]
        TBool

checkExpression :: [(Bool, (Text, [ann]))] -> Type -> Either [SemanticError ann] Type
checkExpression errors ty =
    let
        applicableErrors = map snd (filter fst errors)
    in
        case applicableErrors of
            [] -> Right ty
            _ -> Left (map (uncurry (flip SemanticError)) applicableErrors)

typeOf :: Context -> Expression ann -> Either [SemanticError ann] Type
typeOf context (Addition ann l r) = checkNumericBinOp context l r
typeOf context (Subtraction ann l r) = checkNumericBinOp context l r
typeOf context (Multiplication ann l r) = checkNumericBinOp context l r
typeOf context (Division ann l r) = checkNumericBinOp context l r
typeOf context (Conjunction ann l r) = checkBoolBinOp context l r
typeOf context (Disjunction ann l r) = checkBoolBinOp context l r
typeOf context (Equality ann l r) = checkEqBinOp context l r
typeOf context (Inequality ann l r) = checkEqBinOp context l r
typeOf context (LessThan ann l r) = checkOrdBinOp context l r
typeOf context (LessThanEqual ann l r) = checkOrdBinOp context l r
typeOf context (Greater ann l r) = checkOrdBinOp context l r
typeOf context (GreaterThanEqual ann l r) = checkOrdBinOp context l r
typeOf context (Number ann _) = pure TInt32
typeOf context (Boolean ann _) = pure TBool
typeOf context (Variable ann name) =
        maybeToEither ([SemanticError [ann] ("Variable " <> name <> " not defined.")]) (contextLookupBinding name context)
typeOf context (Call ann name args) = do
    fn <- maybeToEither ([SemanticError [ann] ("Function ‘" <> name <> "’ not defined.")]) (contextLookupBinding name context)

    case fn of
        Function fnargs result -> do
            when (length fnargs /= length args) (Left [SemanticError [ann] ("Function ‘" <> "’ expects " <> tshow (length fnargs) <> " arguments but " <> tshow (length args) <> " were given.")])
            let argChecks = zipWith3 (\expected arg n -> typeOf context arg >>= \actual -> if expected == actual then Right expected else Left [SemanticError [ann] ("Argument " <> tshow n <> " of function ‘" <> name <> "’ requires a value of type ‘" <> ppType expected <> "’ but ‘" <> ppType actual <> "’ was received.")]) fnargs args [1..]
            if all isRight argChecks then Right result else head (filter isLeft argChecks)
        _ -> Left [SemanticError [ann] ("Name ‘" <> name <> "’ is not a function.")]

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither a (Just b) = Right b
maybeToEither a Nothing = Left a
