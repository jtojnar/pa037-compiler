{-# LANGUAGE OverloadedStrings #-}

module Parser (program, expression) where

import Ast
import Control.Applicative ((<|>), optional, some, many)
import Data.Char (isDigit, ord)
import Data.Text (Text)
import Data.Void
import qualified Data.Text as T
import Text.Megaparsec ((<?>), Parsec, SourcePos, eof, getSourcePos, sepBy, sourceName, sourceLine, sourceColumn, unPos)
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, space, string)

default (Text)

-- Attoparsec/Parsec compatibility

newtype Pos = Pos { sp :: SourcePos }

instance Show Pos where
    show (Pos sp) = "'" <> sourceName sp <> ":" <> show (unPos (sourceLine sp)) <> ":" <> show (unPos (sourceColumn sp)) <> "'"

pos = Pos <$> getSourcePos

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
word  = T.pack <$> some (alphaNumChar <|> char '_') <* skipSpace

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

program :: Parser (Program Pos)
program = skipSpace *> many funDef <* endOfInput

arguments :: Parser [(Identifier, Type)]
arguments = ((,) <$> identifier <* text ":" <*> typeVal) `sepBy` text ","

funDef :: Parser (Identifier, FunctionDefinition Pos)
funDef = (,) <$> (text "fn" *> identifier) <*> (text "(" *> ((,,) <$> arguments <*> (text ")" *> text "->" *> typeVal) <*> (text "{" *> commands) <* text "}"))

commands :: Parser (Commands Pos)
commands = many command

block :: Parser (Commands Pos)
block = text "{" *> commands <* text "}"

command :: Parser (Command Pos)
command
    = (Conditional <$> pos <*> ((:) <$> ((,) <$> (text "if" *> expression) <*> block) <*> many ((,) <$> (text "elseif" *> expression) <*> block)) <*> optional (text "else" *> block) <?> "conditional statement")
    <|> (ForEach <$> pos <*> (text "for" *> identifier) <*> (text "in" *> expression) <*> block <?> "for..in loop")
    <|> (While <$> pos <*> (text "while" *> expression) <*> block <?> "while loop")
    <|> (Return <$> pos <*> (text "return" *> expression) <* text ";" <?> "return statement")
    <|> (Declaration <$> pos <*> (text "let" *> identifier) <*> (text ":" *> typeVal) <*> (optional (text "=" *> expression)) <* text ";" <?> "declaration")
    <|> ((\ann name p -> p ann name) <$> pos <*> identifier <*> (((text "=" *> fmap (\expr -> \ann name -> Assignment ann name expr) expression) <?> "assignment") <|> ((text "(" *> fmap (\args -> \ann name -> CCall ann name args) (expression `sepBy` text ",") <* text ")") <?> "function call")) <* text ";")

{-| Expressions formed by binary operators with priority 2
-}
expression :: Parser (Expression Pos)
expression
    = expression3 `chainl1` operators
    where
        operators
            = text "||" *> (Disjunction <$> pos)

{-| Expressions formed by binary operators with priority 3
-}
expression3 :: Parser (Expression Pos)
expression3
    = expression4 `chainl1` operators
    where
        operators
            = text "&&" *> (Conjunction <$> pos)

{-| Expressions formed by binary operators with priority 4
-}
expression4 :: Parser (Expression Pos)
expression4
    = expression6 `chainl1` operators
    where
        operators
            = text "==" *> (Equality <$> pos)
            <|> text "!=" *> (Inequality <$> pos)
            <|> text "<=" *> (LessThanEqual <$> pos)
            <|> text "<" *> (LessThan <$> pos)
            <|> text ">=" *> (GreaterThanEqual <$> pos)
            <|> text ">" *> (Greater <$> pos)


{-| Expressions formed by binary operators with priority 6
-}
expression6 :: Parser (Expression Pos)
expression6
    = expression7 `chainl1` operators
    where
        operators
            = text "+" *> (Addition <$> pos)
            <|> text "-" *> (Subtraction <$> pos)

{-| Expressions formed by binary operators with priority 7
-}
expression7 :: Parser (Expression Pos)
expression7
    = atom `chainl1` operators
    where
        operators
            = text "*" *> (Multiplication <$> pos)
            <|> text "/" *> (Division <$> pos)

{-| Identifier can refer to a variable when by itself, or a function name when followed by a list of arguments -}
callOrUse :: Pos -> Identifier -> Maybe [Expression Pos] -> Expression Pos
callOrUse ann name (Just args) = Call ann name args
callOrUse ann name Nothing = Variable ann name

{-| Atomic expressions and unary operator -}
atom :: Parser (Expression Pos)
atom
    = text "(" *> expression <* text ")"
    <|> Negation <$> pos <*> (text "!" *> atom)
    <|> Number <$> pos <*> int
    <|> Boolean <$> pos <*> bool
    <|> callOrUse <$> pos <*> identifier <*> optional (text "(" *> (expression `sepBy` text ",") <* text ")")
