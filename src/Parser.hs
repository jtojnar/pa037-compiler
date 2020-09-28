{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Parser (Pos(..), command, program, expression, pos) where

import Ast
import Control.Applicative ((<|>), liftA2, optional, some, many)
import Data.Char (isDigit, ord)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Void
import qualified Data.Text as T
import Text.Megaparsec ((<?>), Parsec, SourcePos, eof, getOffset, getSourcePos, manyTill, parseError, satisfy, sepBy, sourceName, sourceLine, sourceColumn, unPos)
import Text.Megaparsec.Error (ErrorItem(..), ParseError(..))
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

{-| Parse a “lexeme”. Throughout our parser each parser is responsible
for consuming the space that follows it, this function makes it almost a  -}
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
    <|> pure parenthisedTypes <*> (text "(" *> (typeVal `sepBy` text ",") <* text ")")
    <|> pure TPtr <* text "ptr" <*> typeVal
    <|> flip TArray <$> (text "[" *> typeVal <* text ";") <*> (expression (return ()) <* text "]") -- TODO: wire in annotations
    <|> pure TBool <* text "bool") <?> "type")

program :: Parser ann -> Parser (Program ann)
program annp = skipSpace *> many ((funDef annp <|> extern annp)) <* endOfInput

arguments :: Parser [(Identifier, Type)]
arguments = ((,) <$> identifier <* text ":" <*> typeVal) `sepBy` text ","

funDef :: Parser ann -> Parser (Identifier, FunctionDefinition ann)
funDef annp = handleFunDef <$> (text "fn" *> identifier) <*> (text "(" *> arguments) <*> (text ")" *> text "->" *> typeVal) <*> (text "{" *> commands annp) <*> annp <* text "}"
  where
    handleFunDef name arguments result body endAnn = (name, FunctionDefinition endAnn arguments result False (Just body))

{-| Define external function.
Unlike regular functions, these can be variadic. -}
extern :: Parser ann -> Parser (Identifier, FunctionDefinition ann)
extern annp = handleExtern <$> ((text "variadic" *> pure True) <|> pure False) <*> (text "extern" *> identifier) <*> (text "(" *> arguments) <*> (text ")" *> text "->" *> typeVal) <*> annp <* text ";"
  where
    handleExtern variadic name arguments result endAnn = (name, FunctionDefinition endAnn arguments result variadic Nothing)

commands :: Parser ann -> Parser (Commands ann)
commands annp = many (command annp)

block :: Parser ann -> Parser (Commands ann)
block annp = text "{" *> commands annp <* text "}"

infixl 4 <*.*>
(<*.*>) :: Applicative f => f (a -> b -> c) -> f (a, b) -> f c
(<*.*>) = liftA2 uncurry

command :: Parser ann -> Parser (Command ann)
command annp
    = (Conditional <$> annp <*> ((:) <$> ((,) <$> (text "if" *> expression annp) <*> block annp) <*> many ((,) <$> (text "elseif" *> expression annp) <*> block annp)) <*> optional (text "else" *> block annp) <?> "conditional statement")
    <|> (ForEach <$> annp <*> (text "for" *> identifier) <*> (text "in" *> expression annp) <*> block annp <?> "for..in loop")
    <|> (While <$> annp <*> (text "while" *> expression annp) <*> block annp <?> "while loop")
    <|> (Return <$> annp <*> (text "return" *> expression annp) <* text ";" <?> "return statement")
    <|> (Declaration <$> annp <*> (text "let" *> identifier) <*.*> (((,) <$> (text ":" *> typeVal) <*> optional (assignRight annp)) <|> ((,) <$> pure TBot <*> (Just <$> assignRight annp))) <* text ";" <?> "declaration")
    <|> callOrAssign annp

assignRight :: Parser ann -> Parser (Expression ann)
assignRight annp = text "=" *> expression annp

{-| Command starting with an identifier can be either assignment or a function call.
We have to decide based no whether there is an equal sign so we need to use monadic parser. -}
callOrAssign :: Parser ann -> Parser (Command ann)
callOrAssign annp = do
    ann <- annp
    lhs <- lvalue annp
    semicolonLocation <- getOffset
    isAssignment <- (text ";" *> pure False) <|> (text "=" *> pure True)
    if isAssignment then do
        rhs <- expression annp <* text ";"
        return (Assignment ann lhs rhs)
    else case lhs of
        -- The lvalue can be one of several expressions but the grammar of the language only allows function calls.
        -- We also need to repack the outermost call from an expression to a command.
        Call ann' callee args -> return (CCall ann' callee args)
        _ ->
            let
                es = [Label (NE.fromList "function arguments"), Label (NE.fromList "assignment")]
            in
                parseError (TrivialError semicolonLocation (Just (Label (NE.fromList ";"))) (Set.fromList es))

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

{-| Atomic expressions and unary operator -}
atom :: Parser ann -> Parser (Expression ann)
atom annp
    = text "(" *> expression annp <* text ")"
    <|> Negation <$> annp <*> (text "!" *> atom annp)
    <|> AddressOf <$> annp <*> (text "&" *> atom annp)
    <|> Number <$> annp <*> int
    <|> Boolean <$> annp <*> bool
    <|> Character <$> annp <*> (char '\'' *> charLiteral <* char '\'') <* skipSpace -- We are not using @text@ parser so we need to skip the following whitespace manually.
    <|> String <$> annp <*> (char '"' *> (T.pack <$> manyTill charLiteral (char '"'))) <* skipSpace
    <|> lvalue annp

{-| Parse a lvalue, an expression that can be assigned to. -}
lvalue :: Parser ann -> Parser (Expression ann)
lvalue annp = callOrUse <$> annp <*> identifier <*> many (callArgs annp <|> arrayAccessor annp)

{-| Identifier can refer to a variable when by itself,
a function name when followed by a list of arguments in parentheses,
or an array when followed by square brackets.
It is also possible to access multi-dimensional arrays,
or even call item of an array that was returned by a function. -}
callOrUse :: ann -> Identifier -> [Expression ann -> Expression ann] -> Expression ann
callOrUse ann name actions = foldl (\expr action -> action expr) (Variable ann name) actions

callArgs, arrayAccessor :: Parser ann -> Parser (Expression ann -> Expression ann)
callArgs annp = flip . Call <$> annp <*> (text "(" *> (expression annp `sepBy` text ",") <* text ")") <?> "function arguments"
arrayAccessor annp = flip . ArrayAccess <$> annp <*> (text "[" *> expression annp <* text "]") <?> "array accessor"
