{-# LANGUAGE OverloadedStrings #-}

module Printer (ppExpr, ppType) where

import Ast
import Data.Text (Text)
import qualified Data.Text as T
import Helpers

default (Text)

{- Pretty print a correctly parenthesized expression. -}
ppExpr :: Expression ann -> Text
ppExpr = ppExpr' 0

parenthesize :: Int -> Int -> Text -> Text
parenthesize outer inner expr = if outer > inner then "(" <> expr <> ")" else expr

ppExpr' :: Int -> Expression ann -> Text
ppExpr' fixity expr@(Addition _ann l r) = parenthesize fixity (expressionFixity expr) $ ppExpr' (expressionFixity expr) l <> " + " <> ppExpr' (expressionFixity expr) r
ppExpr' fixity expr@(Subtraction _ann l r) = parenthesize fixity (expressionFixity expr) $ ppExpr' (expressionFixity expr) l <> " - " <> ppExpr' (expressionFixity expr) r
ppExpr' fixity expr@(Multiplication _ann l r) = parenthesize fixity (expressionFixity expr) $ ppExpr' (expressionFixity expr) l <> " * " <> ppExpr' (expressionFixity expr) r
ppExpr' fixity expr@(Division _ann l r) = parenthesize fixity (expressionFixity expr) $ ppExpr' (expressionFixity expr) l <> " / " <> ppExpr' (expressionFixity expr) r
ppExpr' fixity expr@(Conjunction _ann l r) = parenthesize fixity (expressionFixity expr) $ ppExpr' (expressionFixity expr) l <> " && " <> ppExpr' (expressionFixity expr) r
ppExpr' fixity expr@(Disjunction _ann l r) = parenthesize fixity (expressionFixity expr) $ ppExpr' (expressionFixity expr) l <> " || " <> ppExpr' (expressionFixity expr) r
ppExpr' fixity expr@(Negation _ann inner) = parenthesize fixity (expressionFixity expr) $ ppExpr' (expressionFixity expr) inner
ppExpr' fixity expr@(Equality _ann l r) = parenthesize fixity (expressionFixity expr) $ ppExpr' (expressionFixity expr) l <> " == " <> ppExpr' (expressionFixity expr) r
ppExpr' fixity expr@(Inequality _ann l r) = parenthesize fixity (expressionFixity expr) $ ppExpr' (expressionFixity expr) l <> " != " <> ppExpr' (expressionFixity expr) r
ppExpr' fixity expr@(LessThan _ann l r) = parenthesize fixity (expressionFixity expr) $ ppExpr' (expressionFixity expr) l <> " < " <> ppExpr' (expressionFixity expr) r
ppExpr' fixity expr@(LessThanEqual _ann l r) = parenthesize fixity (expressionFixity expr) $ ppExpr' (expressionFixity expr) l <> " <= " <> ppExpr' (expressionFixity expr) r
ppExpr' fixity expr@(Greater _ann l r) = parenthesize fixity (expressionFixity expr) $ ppExpr' (expressionFixity expr) l <> " > " <> ppExpr' (expressionFixity expr) r
ppExpr' fixity expr@(GreaterThanEqual _ann l r) = parenthesize fixity (expressionFixity expr) $ ppExpr' (expressionFixity expr) l <> " >= " <> ppExpr' (expressionFixity expr) r
ppExpr' fixity expr@(Number _ann val) = parenthesize fixity (expressionFixity expr) $ tshow val
ppExpr' fixity expr@(Boolean _ann True) = parenthesize fixity (expressionFixity expr) $ "true"
ppExpr' fixity expr@(Boolean _ann False) = parenthesize fixity (expressionFixity expr) $ "false"
ppExpr' fixity expr@(Character _ann val) = parenthesize fixity (expressionFixity expr) $ "'" <> T.pack [val] <> "'"
ppExpr' fixity expr@(String _ann val) = parenthesize fixity (expressionFixity expr) $ "\"" <> val <> "\""
ppExpr' fixity expr@(Variable _ann name) = parenthesize fixity (expressionFixity expr) $ name
ppExpr' fixity expr@(Call _ann callee args) = parenthesize fixity (expressionFixity expr) $ ppExpr' (expressionFixity expr) callee <> "(" <> (T.intercalate ", " (map (ppExpr' (expressionFixity expr)) args)) <> ")"
ppExpr' fixity expr@(ArrayAccess _ann array index) = parenthesize fixity (expressionFixity expr) $ ppExpr' (expressionFixity expr) array <> "[" <> ppExpr' (expressionFixity expr) index <> "]"
ppExpr' fixity expr@(ProductFieldAccess _ann prod name) = parenthesize fixity (expressionFixity expr) $ ppExpr' (expressionFixity expr) prod <> "." <> name
ppExpr' fixity expr@(AddressOf _ann e) = parenthesize fixity (expressionFixity expr) $ "&" <> ppExpr' (expressionFixity expr) e

ppType :: Type -> Text
ppType TBot = "⊥"
ppType TInt32 = "i32"
ppType TChar = "char"
ppType TBool = "bool"
ppType (TPtr t) = "ptr " <> ppType t
ppType (TArray (Number _ size) t) = "[" <> ppType t <> "; " <> tshow size <> "]"
ppType (TArray sizeExpr t) = "[" <> ppType t <> "; " <> ppExpr sizeExpr <> "]"
ppType (TProduct members) = "{" <> T.intercalate "," (map (\(name, t) -> name <> ":" <> ppType t) members) <> "}"
ppType (Function args result variadic) = "fn(" <> T.intercalate ", " (map ppType args) <> (if variadic then ", …" else "") <> ") -> " <> ppType result
