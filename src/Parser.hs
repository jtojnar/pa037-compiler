{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Parser (Pos, command, program, expression, pos) where

import Ast
import Control.Applicative ((<|>), optional, some, many)
import Data.Char (isDigit, ord)
import Data.Text (Text)
import Data.Void
import qualified Data.Text as T
import Text.Megaparsec ((<?>), Parsec, SourcePos, eof, getSourcePos, manyTill, satisfy, sepBy, sourceName, sourceLine, sourceColumn, unPos)
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, space1, string)
import Text.Megaparsec.Char.Lexer (charLiteral, space, skipLineComment, skipBlockCommentNested)

default (Text)

-- Attoparsec/Parsec compatibility

newtype Pos = Pos { sp :: SourcePos }

instance Show Pos where
    show (Pos sp) = "'" <> sourceName sp <> ":" <> show (unPos (sourceLine sp)) <> ":" <> show (unPos (sourceColumn sp)) <> "'"

pos :: Parser Pos
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
skipSpace =
    space
        space1
        (skipLineComment "//")
        (skipBlockCommentNested "/*" "*/")

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

parenthisedTypes :: [Type] -> Type
parenthisedTypes [] = TNil
parenthisedTypes [ty] = ty
parenthisedTypes tup = error "Tuples not yet supported."

typeVal :: Parser Type
typeVal
    = ((pure TInt32 <* text "i32"
    <|> pure TChar <* text "char"
    <|> pure parenthisedTypes <*> (text "(" *> (typeVal `sepBy` text ",") <* ")")
    <|> pure TPtr <* text "ptr" <*> typeVal
    <|> pure TBool <* text "bool") <?> "type") <* skipSpace

program :: Parser ann -> Parser (Program ann)
program annp = skipSpace *> many ((funDef annp <|> extern)) <* endOfInput

arguments :: Parser [(Identifier, Type)]
arguments = ((,) <$> identifier <* text ":" <*> typeVal) `sepBy` text ","

funDef :: Parser ann -> Parser (Identifier, FunctionDefinition ann)
funDef annp = handleFunDef <$> (text "fn" *> identifier) <*> (text "(" *> arguments) <*> (text ")" *> text "->" *> typeVal) <*> (text "{" *> commands annp) <* text "}"
  where
    handleFunDef name arguments result body = (name, FunctionDefinition arguments result False (Just body))

{-| Define external function.
Unlike regular functions, these can be variadic. -}
extern :: Parser (Identifier, FunctionDefinition ann)
extern = handleExtern <$> ((text "variadic" *> pure True) <|> pure False) <*> (text "extern" *> identifier) <*> (text "(" *> arguments) <*> (text ")" *> text "->" *> typeVal) <* text ";"
  where
    handleExtern variadic name arguments result = (name, FunctionDefinition arguments result variadic Nothing)

commands :: Parser ann -> Parser (Commands ann)
commands annp = many (command annp)

block :: Parser ann -> Parser (Commands ann)
block annp = text "{" *> commands annp <* text "}"

command :: Parser ann -> Parser (Command ann)
command annp
    = (Conditional <$> annp <*> ((:) <$> ((,) <$> (text "if" *> expression annp) <*> block annp) <*> many ((,) <$> (text "elseif" *> expression annp) <*> block annp)) <*> optional (text "else" *> block annp) <?> "conditional statement")
    <|> (ForEach <$> annp <*> (text "for" *> identifier) <*> (text "in" *> expression annp) <*> block annp <?> "for..in loop")
    <|> (While <$> annp <*> (text "while" *> expression annp) <*> block annp <?> "while loop")
    <|> (Return <$> annp <*> (text "return" *> expression annp) <* text ";" <?> "return statement")
    <|> (Declaration <$> annp <*> (text "let" *> identifier) <*> (text ":" *> typeVal) <*> (optional (text "=" *> expression annp)) <* text ";" <?> "declaration")
    <|> (callOrAssign <$> annp <*> identifier <*> (assignment annp <|> ccall annp) <* text ";")

{-| Command starting with an identifier can be either assignment or a function call.
We have to decide based no whether there is an equal sign.-}
callOrAssign :: ann -> Identifier -> (ann -> Identifier -> Command ann) -> Command ann
callOrAssign ann name p = p ann name

{-| Parse a portion of assignment command after the identifier. -}
assignment :: Parser ann -> Parser (ann -> Identifier -> Command ann)
assignment annp = (text "=" *> fmap (\expr -> \ann name -> Assignment ann name expr) (expression annp)) <?> "assignment"

{-| Parse a portion of call command after the identifier. -}
ccall :: Parser ann -> Parser (ann -> Identifier -> Command ann)
ccall annp = do
    actions <- some (callArgs annp) <?> "function call"
    return (\ann name ->
        let
            -- The callArgs parser returns (Expr -> Expr) so we need to build the call chain.
            Call ann' callee args = callOrUse ann name actions
        in
            -- We also need to repack the outermost call from an expression to a command.
            CCall ann' callee args)

{-| Expressions formed by binary operators with priority 2
-}
expression :: Parser ann -> Parser (Expression ann)
expression annp
    = expression3 annp `chainl1` operators
    where
        operators
            = text "||" *> (Disjunction <$> annp)

{-| Expressions formed by binary operators with priority 3
-}
expression3 :: Parser ann -> Parser (Expression ann)
expression3 annp
    = expression4 annp `chainl1` operators
    where
        operators
            = text "&&" *> (Conjunction <$> annp)

{-| Expressions formed by binary operators with priority 4
-}
expression4 :: Parser ann -> Parser (Expression ann)
expression4 annp
    = expression6 annp `chainl1` operators
    where
        operators
            = text "==" *> (Equality <$> annp)
            <|> text "!=" *> (Inequality <$> annp)
            <|> text "<=" *> (LessThanEqual <$> annp)
            <|> text "<" *> (LessThan <$> annp)
            <|> text ">=" *> (GreaterThanEqual <$> annp)
            <|> text ">" *> (Greater <$> annp)


{-| Expressions formed by binary operators with priority 6
-}
expression6 :: Parser ann -> Parser (Expression ann)
expression6 annp
    = expression7 annp `chainl1` operators
    where
        operators
            = text "+" *> (Addition <$> annp)
            <|> text "-" *> (Subtraction <$> annp)

{-| Expressions formed by binary operators with priority 7
-}
expression7 :: Parser ann -> Parser (Expression ann)
expression7 annp
    = atom annp `chainl1` operators
    where
        operators
            = text "*" *> (Multiplication <$> annp)
            <|> text "/" *> (Division <$> annp)

{-| Identifier can refer to a variable when by itself,
a function name when followed by a list of arguments in parentheses,
or an array when followed by square brackets.
It is also possible to access multi-dimensional arrays,
or even call item of an array that was returned by a function. -}
callOrUse :: ann -> Identifier -> [Expression ann -> Expression ann] -> Expression ann
callOrUse ann name actions = foldl (\expr action -> action expr) (Variable ann name) actions


{-| Atomic expressions and unary operator -}
atom :: Parser ann -> Parser (Expression ann)
atom annp
    = text "(" *> expression annp <* text ")"
    <|> Negation <$> annp <*> (text "!" *> atom annp)
    <|> Number <$> annp <*> int
    <|> Boolean <$> annp <*> bool
    <|> Character <$> annp <*> (char '\'' *> charLiteral <* char '\'')
    <|> String <$> annp <*> (char '"' *> (T.pack <$> manyTill charLiteral (char '"')))
    <|> callOrUse <$> annp <*> identifier <*> many (callArgs annp <|> arrayAccessor annp)


callArgs, arrayAccessor :: Parser ann -> Parser (Expression ann -> Expression ann)
callArgs annp = flip . Call <$> annp <*> (text "(" *> (expression annp `sepBy` text ",") <* text ")")
arrayAccessor annp = flip . ArrayAccess <$> annp <*> (text "[" *> expression annp <* text "]")
