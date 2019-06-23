module Ast where

import Data.Text (Text)

data Type = TInt32 | TChar | TBool
    deriving (Eq, Show)

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
    | Variable ann Identifier
    | Call ann Identifier [(Expression ann)]
    deriving (Eq, Show)

type Identifier = Text

type Program ann = [(Identifier, FunctionDefinition ann)]

type BinaryOperator ann = Expression ann -> Expression ann -> Expression ann

type FunctionDefinition ann = ([(Identifier, Type)], Type, Commands ann)

type Commands ann = [Command ann]

data Command ann
    = Conditional ann [(Expression ann, Commands ann)] (Maybe (Commands ann))
    | ForEach ann Identifier (Expression ann) (Commands ann)
    | While ann (Expression ann) (Commands ann)
    | Return ann (Expression ann)
    | Declaration ann Identifier Type (Maybe (Expression ann))
    | Assignment ann Identifier (Expression ann)
    | CCall ann Identifier [(Expression ann)]
    deriving (Eq, Show)
