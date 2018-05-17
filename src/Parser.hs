{-# LANGUAGE OverloadedStrings #-}

module Parser (program) where

import Ast
import Control.Applicative ((<|>), optional, some, many)
import Data.Char (isDigit, ord)
import Data.Text (Text)
import Data.Void
import qualified Data.Text as T
import Text.Parsec ((<?>), char, digit, eof, letter, sepBy, spaces, string, try)
import Text.Parsec.Text (Parser)

default (Text)

-- Attoparsec compatibility

decimal :: Integral a => Parser a
decimal = foldl step 0 <$> some digit
    where step a c = a * 10 + fromIntegral (ord c - 48)

signed :: Num a => Parser a -> Parser a
signed p = (negate <$> (char '-' *> p))
       <|> (char '+' *> p)
       <|> p

endOfInput :: Parser ()
endOfInput = eof

skipSpace :: Parser ()
skipSpace = spaces

-- HELPERS

word :: Parser Text
word  = T.pack <$> some letter <* skipSpace

text :: String -> Parser String
text s = string s <* skipSpace

-- LEXEMES

identifier :: Parser Identifier
identifier = word <* skipSpace <?> "identifier"

int :: Parser Int
int = signed decimal <* skipSpace <?> "number"

bool :: Parser Bool
bool = (string "true" *> pure True <|> string "false" *> pure False) <* skipSpace <?> "boolean"

-- NON-TERMINALS

typeVal :: Parser Type
typeVal
    = (pure TInt32 <* text "int32"
    <|> pure TChar <* text "char"
    <|> pure TBool <* text "bool") <* skipSpace <?> "type"

program :: Parser Program
program = skipSpace *> many funDef <* endOfInput

arguments :: Parser [(Identifier, Type)]
arguments = ((,) <$> identifier <* text ":" <*> typeVal) `sepBy` text ","

funDef :: Parser (Identifier, FunctionDefinition)
funDef = (,) <$> (text "fn" *> identifier) <*> (text "(" *> ((,,) <$> arguments <*> (text ")" *> text "->" *> typeVal) <*> (text "{" *> commands) <* text "}"))

commands :: Parser Commands
commands = command `sepBy` text ";"

block :: Parser Commands
block = text "{" *> commands <* text "}"

command :: Parser Command
command
    = try (Conditional <$> ((:) <$> ((,) <$> (text "if" *> expression) <*> block) <*> many ((,) <$> (text "elseif" *> expression) <*> block)) <*> optional (text "else" *> block))
    <|> try (ForEach <$> (text "for" *> identifier) <*> (text "in" *> expression) <*> block)
    <|> try (While <$> (text "while" *> expression) <*> block)
    <|> Assignment <$> identifier <*> (text ":" *> typeVal) <*> ((text "=" <?> "assignment operator") *> expression)

{-| Expressions formed by binary operators with priority 2
-}
expression :: Parser Expression
expression
    = try (Disjunction <$> expression3 <* text "||" <*> expression)
    <|> expression3

{-| Expressions formed by binary operators with priority 3
-}
expression3 :: Parser Expression
expression3
    = try (Conjunction <$> expression4 <* text "&&" <*> expression)
    <|> expression4

{-| Expressions formed by binary operators with priority 4
-}
expression4 :: Parser Expression
expression4
    = try (Equality <$> expression6 <* text "==" <*> expression)
    <|> try (Inequality <$> expression6 <* text "!=" <*> expression)
    <|> try (LessThan <$> expression6 <* text "<" <*> expression)
    <|> try (LessThanEqual <$> expression6 <* text "<=" <*> expression)
    <|> try (Greater  <$> expression6 <* text ">" <*> expression)
    <|> try (GreaterThanEqual <$> expression6 <* text ">=" <*> expression)
    <|> expression6

{-| Expressions formed by binary operators with priority 6
-}
expression6 :: Parser Expression
expression6
    = try (Addition <$> expression7 <* text "+" <*> expression)
    <|> try (Subtraction <$> expression7 <* text "-" <*> expression)
    <|> expression7

{-| Expressions formed by binary operators with priority 7
-}
expression7 :: Parser Expression
expression7
    = try (Multiplication <$> atom <* text "*" <*> expression)
    <|> try (Division <$> atom <* text "/" <*> expression)
    <|> atom

{-| Atomic expressions and unary operator -}
atom :: Parser Expression
atom
    = text "(" *> expression <* text ")"
    <|> Negation <$> (text "!" *> expression)
    <|> Number <$> int
    <|> try (Boolean <$> bool)
    <|> Variable <$> identifier
