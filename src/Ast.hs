{-# LANGUAGE RecordWildCards #-}

module Ast where

import Data.Text (Text)

data Type = TBot | TInt32 | TChar | TBool | TNil | TPtr Type | Function [Type] Type Bool
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
tEquals l r = l == r

data Expression ann
    = Addition ann (Expression ann) (Expression ann)
    | Subtraction ann (Expression ann) (Expression ann)
    | Multiplication ann (Expression ann) (Expression ann)
    | Division ann (Expression ann) (Expression ann)
    | Conjunction ann (Expression ann) (Expression ann)
    | Disjunction ann (Expression ann) (Expression ann)
    | Negation ann (Expression ann)
    | Equality ann (Expression ann) (Expression ann)
    | Inequality ann (Expression ann) (Expression ann)
    | LessThan ann (Expression ann) (Expression ann)
    | LessThanEqual ann (Expression ann) (Expression ann)
    | Greater ann (Expression ann) (Expression ann)
    | GreaterThanEqual ann (Expression ann) (Expression ann)
    | Number ann Int
    | Boolean ann Bool
    | Character ann Char
    | String ann Text
    | Variable ann Identifier
    | Call ann (Expression ann) [(Expression ann)]
    | ArrayAccess ann (Expression ann) (Expression ann)
    deriving (Eq, Show)

expressionAnn :: Expression ann -> ann
expressionAnn (Addition ann _ _) = ann
expressionAnn (Subtraction ann _ _) = ann
expressionAnn (Multiplication ann _ _) = ann
expressionAnn (Division ann _ _) = ann
expressionAnn (Conjunction ann _ _) = ann
expressionAnn (Disjunction ann _ _) = ann
expressionAnn (Negation ann _) = ann
expressionAnn (Equality ann _ _) = ann
expressionAnn (Inequality ann _ _) = ann
expressionAnn (LessThan ann _ _) = ann
expressionAnn (LessThanEqual ann _ _) = ann
expressionAnn (Greater ann _ _) = ann
expressionAnn (GreaterThanEqual ann _ _) = ann
expressionAnn (Number ann _) = ann
expressionAnn (Boolean ann _) = ann
expressionAnn (Character ann _) = ann
expressionAnn (String ann _) = ann
expressionAnn (Variable ann _) = ann
expressionAnn (Call ann _ _) = ann
expressionAnn (ArrayAccess ann _ _) = ann

stripAnn :: Expression ann -> Expression ()
stripAnn (Addition ann l r) = Addition () (stripAnn l) (stripAnn r)
stripAnn (Subtraction ann l r) = Subtraction () (stripAnn l) (stripAnn r)
stripAnn (Multiplication ann l r) = Multiplication () (stripAnn l) (stripAnn r)
stripAnn (Division ann l r) = Division () (stripAnn l) (stripAnn r)
stripAnn (Conjunction ann l r) = Conjunction () (stripAnn l) (stripAnn r)
stripAnn (Disjunction ann l r) = Disjunction () (stripAnn l) (stripAnn r)
stripAnn (Negation ann e) = Negation () (stripAnn e)
stripAnn (Equality ann l r) = Equality () (stripAnn l) (stripAnn r)
stripAnn (Inequality ann l r) = Inequality () (stripAnn l) (stripAnn r)
stripAnn (LessThan ann l r) = LessThan () (stripAnn l) (stripAnn r)
stripAnn (LessThanEqual ann l r) = LessThanEqual () (stripAnn l) (stripAnn r)
stripAnn (Greater ann l r) = Greater () (stripAnn l) (stripAnn r)
stripAnn (GreaterThanEqual ann l r) = GreaterThanEqual () (stripAnn l) (stripAnn r)
stripAnn (Number ann v) = Number () v
stripAnn (Boolean ann v) = Boolean () v
stripAnn (Character ann v) = Character () v
stripAnn (String ann v) = String () v
stripAnn (Variable ann n) = Variable () n
stripAnn (Call ann e args) = Call () (stripAnn e) (map stripAnn args)
stripAnn (ArrayAccess ann e i) = ArrayAccess () (stripAnn e) (stripAnn i)

type Identifier = Text

type Program ann = [(Identifier, FunctionDefinition ann)]

type BinaryOperator ann = Expression ann -> Expression ann -> Expression ann

data FunctionDefinition ann = FunctionDefinition {
    funDefArguments :: [(Identifier, Type)],
    funDefResultType :: Type,
    funDefVariadic :: Bool,
    funDefBody :: Maybe (Commands ann)
} deriving (Eq, Show)

funType :: FunctionDefinition ann -> Type
funType FunctionDefinition {..} = Function (map snd funDefArguments) funDefResultType funDefVariadic

type Commands ann = [Command ann]

data Command ann
    = Conditional ann [(Expression ann, Commands ann)] (Maybe (Commands ann))
    | ForEach ann Identifier (Expression ann) (Commands ann)
    | While ann (Expression ann) (Commands ann)
    | Return ann (Expression ann)
    | Declaration ann Identifier Type (Maybe (Expression ann))
    | Assignment ann Identifier (Expression ann)
    | CCall ann (Expression ann) [(Expression ann)]
    deriving (Eq, Show)

{-| Extract annotation from a command -}
commandAnn :: Command ann -> ann
commandAnn (Conditional ann _ _) = ann
commandAnn (ForEach ann _ _ _) = ann
commandAnn (While ann _ _) = ann
commandAnn (Return ann _) = ann
commandAnn (Declaration ann _ _ _) = ann
commandAnn (Assignment ann _ _) = ann
commandAnn (CCall ann _ _) = ann
