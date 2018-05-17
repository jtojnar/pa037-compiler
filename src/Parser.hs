{-# LANGUAGE OverloadedStrings #-}

module Parser (program) where

import Ast
import Control.Applicative ((<|>), optional, some, many)
import Data.Char (isDigit, ord)
import Data.Text (Text)
import Data.Void
import qualified Data.Text as T
import Text.Parsec ((<?>), char, digit, eof, letter, sepBy, spaces, string)
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
    = Conditional <$> ((:) <$> ((,) <$> (text "if" *> expression) <*> block) <*> many ((,) <$> (text "elseif" *> expression) <*> block)) <*> optional (text "else" *> block)
    <|> ForEach <$> (text "for" *> identifier) <*> (text "in" *> expression) <*> block
    <|> While <$> (text "while" *> expression) <*> block
    <|> Assignment <$> identifier <*> (text ":" *> typeVal) <*> (text "=" *> expression)

expression :: Parser Expression
expression
    = text "(" *> expression <* text ")"
    <|> Addition <$> expression <* text "+" <*> expression
    <|> Subtraction <$> expression <* text "-" <*> expression
    <|> Multiplication <$> expression <* text "*" <*> expression
    <|> Division <$> expression <* text "/" <*> expression
    <|> Conjunction <$> expression <* text "&&" <*> expression
    <|> Disjunction <$> expression <* text "||" <*> expression
    <|> Negation <$> (text "!" *> expression)
    <|> Equality <$> expression <* text "==" <*> expression
    <|> Inequality <$> expression <* text "!=" <*> expression
    <|> LessThan <$> expression <* text "<" <*> expression
    <|> LessThanEqual <$> expression <* text "<=" <*> expression
    <|> Greater  <$> expression <* text ">" <*> expression
    <|> GreaterThanEqual <$> expression <* text ">=" <*> expression
    <|> Number <$> int
    <|> Boolean <$> bool
    <|> Variable <$> identifier
