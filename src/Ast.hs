{-# LANGUAGE RecordWildCards #-}

module Ast where

import Data.Text (Text)

data Type = TBot | TInt32 | TChar | TBool | TNil | TPtr Type | TArray (Expression ()) Type | Function [Type] Type Bool
    deriving (Eq, Show)

isNumericType :: Type -> Bool
isNumericType TInt32 = True
isNumericType _ = False

isBooleanType :: Type -> Bool
isBooleanType TBool = True
isBooleanType _ = False

isEqType :: Type -> Bool
isEqType TInt32 = True
isEqType TChar = True
isEqType TBool = True
isEqType TNil = True
isEqType (TPtr _) = True
isEqType (TArray _ _) = True
isEqType _ = False

isOrdType :: Type -> Bool
isOrdType TInt32 = True
isOrdType TChar = True
isOrdType TBool = True
isOrdType _ = False

{-| Standard type equality testing function.
Equality is defined inductively, with the addition that ⊥ can be unified with any type.
-}
tEquals :: Type -> Type -> Bool
tEquals TBot _ = True
tEquals _ TBot = True

-- When static size is specified in both arrays, it needs to match.
tEquals (TArray (Number _ n1) ty1) (TArray (Number _ n2) ty2) = n1 == n2 && ty1 `tEquals` ty2
-- Otherwise, we cannot determine it at build time se we treat the arrays as plain pointers.
tEquals (TArray _size ty) other = tEquals (TPtr ty) other
tEquals other (TArray _size ty) = tEquals other (TPtr ty)

tEquals l r = l == r

data Expression ann
    = Addition { expressionAnn :: ann, expressionLhs :: Expression ann, expressionRhs :: Expression ann }
    | Subtraction { expressionAnn :: ann, expressionLhs :: Expression ann, expressionRhs :: Expression ann }
    | Multiplication { expressionAnn :: ann, expressionLhs :: Expression ann, expressionRhs :: Expression ann }
    | Division { expressionAnn :: ann, expressionLhs :: Expression ann, expressionRhs :: Expression ann }
    | Conjunction { expressionAnn :: ann, expressionLhs :: Expression ann, expressionRhs :: Expression ann }
    | Disjunction { expressionAnn :: ann, expressionLhs :: Expression ann, expressionRhs :: Expression ann }
    | Negation { expressionAnn :: ann, expressionInner :: Expression ann }
    | Equality { expressionAnn :: ann, expressionLhs :: Expression ann, expressionRhs :: Expression ann }
    | Inequality { expressionAnn :: ann, expressionLhs :: Expression ann, expressionRhs :: Expression ann }
    | LessThan { expressionAnn :: ann, expressionLhs :: Expression ann, expressionRhs :: Expression ann }
    | LessThanEqual { expressionAnn :: ann, expressionLhs :: Expression ann, expressionRhs :: Expression ann }
    | Greater { expressionAnn :: ann, expressionLhs :: Expression ann, expressionRhs :: Expression ann }
    | GreaterThanEqual { expressionAnn :: ann, expressionLhs :: Expression ann, expressionRhs :: Expression ann }
    | Number { expressionAnn :: ann, expressionNumVal :: Int }
    | Boolean { expressionAnn :: ann, expressionBoolVal :: Bool }
    | Character { expressionAnn :: ann, expressionCharVal :: Char }
    | String { expressionAnn :: ann, expressionStringVal :: Text }
    | Variable { expressionAnn :: ann, expressionVarName :: Identifier }
    | Call { expressionAnn :: ann, expressionCallee :: Expression ann, expressionArgs :: [Expression ann] }
    | ArrayAccess { expressionAnn :: ann, expressionIndexable :: Expression ann, expressionIndex :: Expression ann }
    deriving (Eq, Show)

mapExpressionAnn :: (ann -> ann') -> Expression ann -> Expression ann'
mapExpressionAnn f (Addition ann l r) = Addition (f ann) (mapExpressionAnn f l) (mapExpressionAnn f r)
mapExpressionAnn f (Subtraction ann l r) = Subtraction (f ann) (mapExpressionAnn f l) (mapExpressionAnn f r)
mapExpressionAnn f (Multiplication ann l r) = Multiplication (f ann) (mapExpressionAnn f l) (mapExpressionAnn f r)
mapExpressionAnn f (Division ann l r) = Division (f ann) (mapExpressionAnn f l) (mapExpressionAnn f r)
mapExpressionAnn f (Conjunction ann l r) = Conjunction (f ann) (mapExpressionAnn f l) (mapExpressionAnn f r)
mapExpressionAnn f (Disjunction ann l r) = Disjunction (f ann) (mapExpressionAnn f l) (mapExpressionAnn f r)
mapExpressionAnn f (Negation ann e) = Negation (f ann) (mapExpressionAnn f e)
mapExpressionAnn f (Equality ann l r) = Equality (f ann) (mapExpressionAnn f l) (mapExpressionAnn f r)
mapExpressionAnn f (Inequality ann l r) = Inequality (f ann) (mapExpressionAnn f l) (mapExpressionAnn f r)
mapExpressionAnn f (LessThan ann l r) = LessThan (f ann) (mapExpressionAnn f l) (mapExpressionAnn f r)
mapExpressionAnn f (LessThanEqual ann l r) = LessThanEqual (f ann) (mapExpressionAnn f l) (mapExpressionAnn f r)
mapExpressionAnn f (Greater ann l r) = Greater (f ann) (mapExpressionAnn f l) (mapExpressionAnn f r)
mapExpressionAnn f (GreaterThanEqual ann l r) = GreaterThanEqual (f ann) (mapExpressionAnn f l) (mapExpressionAnn f r)
mapExpressionAnn f (Number ann v) = Number (f ann) v
mapExpressionAnn f (Boolean ann v) = Boolean (f ann) v
mapExpressionAnn f (Character ann v) = Character (f ann) v
mapExpressionAnn f (String ann v) = String (f ann) v
mapExpressionAnn f (Variable ann n) = Variable (f ann) n
mapExpressionAnn f (Call ann e args) = Call (f ann) (mapExpressionAnn f e) (map (mapExpressionAnn f) args)
mapExpressionAnn f (ArrayAccess ann e i) = ArrayAccess (f ann) (mapExpressionAnn f e) (mapExpressionAnn f i)

type Identifier = Text

type Program ann = [(Identifier, FunctionDefinition ann)]

type BinaryOperator ann = Expression ann -> Expression ann -> Expression ann

data FunctionDefinition ann = FunctionDefinition {
    endAnnotation :: ann,
    funDefArguments :: [(Identifier, Type)],
    funDefResultType :: Type,
    funDefVariadic :: Bool,
    funDefBody :: Maybe (Commands ann)
} deriving (Eq, Show)

funType :: FunctionDefinition ann -> Type
funType FunctionDefinition {..} = Function (map snd funDefArguments) funDefResultType funDefVariadic

type Commands ann = [Command ann]

data Command ann
    = Conditional { commandAnn :: ann, commandBranches :: [(Expression ann, Commands ann)], commandMelse :: Maybe (Commands ann) }
    | ForEach { commandAnn :: ann, commandLoopVar :: Identifier, commandIterable :: Expression ann, commandBody :: Commands ann }
    | While { commandAnn :: ann, commandLoopCond :: Expression ann, commandBody :: Commands ann }
    | Return { commandAnn :: ann, commandReturnVal :: Expression ann }
    | Declaration { commandAnn :: ann, commandVarName :: Identifier, commandVarType :: Type, commandOptAssignment :: Maybe (Expression ann) }
    | Assignment { commandAnn :: ann, commandVarName :: Identifier, commandVarValue :: Expression ann }
    | CCall { commandAnn :: ann, commandCallable :: Expression ann, commandCallArgs :: [Expression ann] }
    deriving (Eq, Show)
