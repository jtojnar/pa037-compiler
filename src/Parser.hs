{-# LANGUAGE OverloadedStrings #-}

module Parser (program, expression) where

import Ast
import Control.Applicative ((<|>), optional, some, many)
import Data.Char (isDigit, ord)
import Data.Text (Text)
import Data.Void
import qualified Data.Text as T
import Text.Megaparsec ((<?>), Parsec, eof, getPosition, sepBy)
import Text.Megaparsec.Char (letterChar, char, digitChar, space, string)

default (Text)

-- Attoparsec/Parsec compatibility

type Parser = Parsec Void Text

decimal :: Integral a => Parser a
decimal = foldl step 0 <$> some digitChar
    where step a c = a * 10 + fromIntegral (ord c - 48)

signed :: Num a => Parser a -> Parser a
signed p = (negate <$> (char '-' *> p))
       <|> (char '+' *> p)
       <|> p

endOfInput :: Parser ()
endOfInput = eof

skipSpace :: Parser ()
skipSpace = space

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= rest
    where
        rest l = (do
            f <- op
            r <- p
            rest (f l r)
          ) <|> pure l

-- HELPERS

word :: Parser Text
word  = T.pack <$> some letterChar <* skipSpace

text :: Text -> Parser Text
text s = string s <* skipSpace

-- LEXEMES

identifier :: Parser Identifier
identifier = (word <?> "identifier") <* skipSpace

int :: Parser Int
int = (signed decimal <?> "number") <* skipSpace

bool :: Parser Bool
bool = ((string "true" *> pure True <|> string "false" *> pure False) <?> "boolean") <* skipSpace

-- NON-TERMINALS

typeVal :: Parser Type
typeVal
    = ((pure TInt32 <* text "int32"
    <|> pure TChar <* text "char"
    <|> pure TBool <* text "bool") <?> "type") <* skipSpace

program :: Parser (Program SourcePos)
program = skipSpace *> many funDef <* endOfInput

arguments :: Parser [(Identifier, Type)]
arguments = ((,) <$> identifier <* text ":" <*> typeVal) `sepBy` text ","

funDef :: Parser (Identifier, FunctionDefinition SourcePos)
funDef = (,) <$> (text "fn" *> identifier) <*> (text "(" *> ((,,) <$> arguments <*> (text ")" *> text "->" *> typeVal) <*> (text "{" *> commands) <* text "}"))

commands :: Parser (Commands SourcePos)
commands = many command

block :: Parser (Commands SourcePos)
block = text "{" *> commands <* text "}"

command :: Parser (Command SourcePos)
command
    = (Conditional <$> getPosition <*> ((:) <$> ((,) <$> (text "if" *> expression) <*> block) <*> many ((,) <$> (text "elseif" *> expression) <*> block)) <*> optional (text "else" *> block) <?> "conditional statement")
    <|> (ForEach <$> getPosition <*> (text "for" *> identifier) <*> (text "in" *> expression) <*> block <?> "for..in loop")
    <|> (While <$> getPosition <*> (text "while" *> expression) <*> block <?> "while loop")
    <|> (Return <$> getPosition <*> (text "return" *> expression) <* text ";" <?> "return statement")
    <|> (Declaration <$> getPosition <*> (text "let" *> identifier) <*> (text ":" *> typeVal) <*> (optional (text "=" *> expression)) <* text ";" <?> "declaration")
    <|> (Assignment <$> getPosition <*> identifier <*> (text "=" *> expression) <* text ";" <?> "assignment")

{-| Expressions formed by binary operators with priority 2
-}
expression :: Parser (Expression SourcePos)
expression
    = getPosition <*> expression3 `chainl1` operators
    where
        operators
            = text "||" *> pure Disjunction

{-| Expressions formed by binary operators with priority 3
-}
expression3 :: Parser (Expression SourcePos)
expression3
    = getPosition <*> expression4 `chainl1` operators
    where
        operators
            = text "&&" *> pure Conjunction

{-| Expressions formed by binary operators with priority 4
-}
expression4 :: Parser (Expression SourcePos)
expression4
    = getPosition <*> expression6 `chainl1` operators
    where
        operators
            = text "==" *> pure Equality
            <|> text "!=" *> pure Inequality
            <|> text "<=" *> pure LessThanEqual
            <|> text "<" *> pure LessThan
            <|> text ">=" *> pure GreaterThanEqual
            <|> text ">" *> pure Greater


{-| Expressions formed by binary operators with priority 6
-}
expression6 :: Parser (Expression SourcePos)
expression6
    = getPosition <*> expression7 `chainl1` operators
    where
        operators
            = text "+" *> pure Addition
            <|> text "-" *> pure Subtraction

{-| Expressions formed by binary operators with priority 7
-}
expression7 :: Parser (Expression SourcePos)
expression7
    = getPosition <*> atom `chainl1` operators
    where
        operators
            = text "*" *> pure Multiplication
            <|> text "/" *> pure Division

{-| Identifier can refer to a variable when by itself, or a function name when followed by a list of arguments -}
callOrUse :: Identifier -> Maybe [Expression SourcePos] -> Expression SourcePos
callOrUse name (Just args) = Call name args
callOrUse name Nothing = Variable name

{-| Atomic expressions and unary operator -}
atom :: Parser (Expression SourcePos)
atom
    = text "(" *> expression <* text ")"
    <|> Negation <$> (text "!" *> atom)
    <|> Number <$> int
    <|> Boolean <$> bool
    <|> callOrUse <$> identifier <*> optional (text "(" *> (expression `sepBy` text ",") <* text ")")
