module Ast where

import Data.Text (Text)

data Type = TInt32 | TChar | TBool
    deriving (Eq, Show)

data Expression
    = Addition Expression Expression
    | Subtraction Expression Expression
    | Multiplication Expression Expression
    | Division Expression Expression
    | Conjunction Expression Expression
    | Disjunction Expression Expression
    | Negation Expression
    | Equality Expression Expression
    | Inequality Expression Expression
    | LessThan Expression Expression
    | LessThanEqual Expression Expression
    | Greater Expression Expression
    | GreaterThanEqual Expression Expression
    | Number Int
    | Boolean Bool
    | Variable Identifier
    deriving (Eq, Show)

type Identifier = Text

type Program = [(Identifier, FunctionDefinition)]

type FunctionDefinition = ([(Identifier, Type)], Type, Commands)

type Commands = [Command]

data Command
    = Conditional [(Expression, Commands)] (Maybe Commands)
    | ForEach Identifier Expression Commands
    | While Expression Commands
    | Return Expression
    | Declaration Identifier Type (Maybe Expression)
    | Assignment Identifier Expression
    deriving (Eq, Show)
