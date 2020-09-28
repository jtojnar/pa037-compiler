{-# LANGUAGE OverloadedStrings #-}

module Printer (ppExpr, ppType) where

import Ast
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Helpers

default (Text)

{- Pretty print a correctly parenthesized expression. -}
ppExpr = ppExpr' 0

parenthesize outer inner expr = if outer > inner then "(" <> expr <> ")" else expr

ppExpr' fixity expr@(Addition ann l r) = parenthesize fixity (expressionFixity expr) $ ppExpr' (expressionFixity expr) l <> " + " <> ppExpr' (expressionFixity expr) r
ppExpr' fixity expr@(Subtraction ann l r) = parenthesize fixity (expressionFixity expr) $ ppExpr' (expressionFixity expr) l <> " - " <> ppExpr' (expressionFixity expr) r
ppExpr' fixity expr@(Multiplication ann l r) = parenthesize fixity (expressionFixity expr) $ ppExpr' (expressionFixity expr) l <> " * " <> ppExpr' (expressionFixity expr) r
ppExpr' fixity expr@(Division ann l r) = parenthesize fixity (expressionFixity expr) $ ppExpr' (expressionFixity expr) l <> " / " <> ppExpr' (expressionFixity expr) r
ppExpr' fixity expr@(Conjunction ann l r) = parenthesize fixity (expressionFixity expr) $ ppExpr' (expressionFixity expr) l <> " && " <> ppExpr' (expressionFixity expr) r
ppExpr' fixity expr@(Disjunction ann l r) = parenthesize fixity (expressionFixity expr) $ ppExpr' (expressionFixity expr) l <> " || " <> ppExpr' (expressionFixity expr) r
ppExpr' fixity expr@(Equality ann l r) = parenthesize fixity (expressionFixity expr) $ ppExpr' (expressionFixity expr) l <> " == " <> ppExpr' (expressionFixity expr) r
ppExpr' fixity expr@(Inequality ann l r) = parenthesize fixity (expressionFixity expr) $ ppExpr' (expressionFixity expr) l <> " != " <> ppExpr' (expressionFixity expr) r
ppExpr' fixity expr@(LessThan ann l r) = parenthesize fixity (expressionFixity expr) $ ppExpr' (expressionFixity expr) l <> " < " <> ppExpr' (expressionFixity expr) r
ppExpr' fixity expr@(LessThanEqual ann l r) = parenthesize fixity (expressionFixity expr) $ ppExpr' (expressionFixity expr) l <> " <= " <> ppExpr' (expressionFixity expr) r
ppExpr' fixity expr@(Greater ann l r) = parenthesize fixity (expressionFixity expr) $ ppExpr' (expressionFixity expr) l <> " > " <> ppExpr' (expressionFixity expr) r
ppExpr' fixity expr@(GreaterThanEqual ann l r) = parenthesize fixity (expressionFixity expr) $ ppExpr' (expressionFixity expr) l <> " >= " <> ppExpr' (expressionFixity expr) r
ppExpr' fixity expr@(Number ann val) = parenthesize fixity (expressionFixity expr) $ tshow val
ppExpr' fixity expr@(Boolean ann True) = parenthesize fixity (expressionFixity expr) $ "true"
ppExpr' fixity expr@(Boolean ann False) = parenthesize fixity (expressionFixity expr) $ "false"
ppExpr' fixity expr@(Character ann val) = parenthesize fixity (expressionFixity expr) $ "'" <> T.pack [val] <> "'"
ppExpr' fixity expr@(String ann val) = parenthesize fixity (expressionFixity expr) $ "\"" <> val <> "\""
ppExpr' fixity expr@(Variable ann name) = parenthesize fixity (expressionFixity expr) $ name
ppExpr' fixity expr@(Call ann callee args) = parenthesize fixity (expressionFixity expr) $ ppExpr' (expressionFixity expr) callee <> "(" <> (T.intercalate ", " (map (ppExpr' (expressionFixity expr)) args)) <> ")"
ppExpr' fixity expr@(ArrayAccess ann array index) = parenthesize fixity (expressionFixity expr) $ ppExpr' (expressionFixity expr) array <> "[" <> ppExpr' (expressionFixity expr) index <> "]"

ppType :: Type -> Text
ppType TBot = "⊥"
ppType TInt32 = "i32"
ppType TChar = "char"
ppType TBool = "bool"
ppType TNil = "()"
ppType (TPtr t) = "ptr " <> ppType t
ppType (TArray (Number _ size) t) = "[" <> ppType t <> "; " <> tshow size <> "]"
ppType (TArray sizeExpr t) = "[" <> ppType t <> "; " <> ppExpr sizeExpr <> "]"
ppType (Function args result variadic) = "fn(" <> T.intercalate ", " (map ppType args) <> (if variadic then ", …" else "") <> ") -> " <> ppType result
