{-# LANGUAGE OverloadedStrings #-}

module Printer (ppExpr, ppType) where

import Ast
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Helpers

default (Text)

ppExpr (Addition ann l r) = ppExpr l <> " + " <> ppExpr r
ppExpr (Subtraction ann l r) = ppExpr l <> " - " <> ppExpr r
ppExpr (Multiplication ann l r) = ppExpr l <> " * " <> ppExpr r
ppExpr (Division ann l r) = ppExpr l <> " / " <> ppExpr r
ppExpr (Conjunction ann l r) = ppExpr l <> " && " <> ppExpr r
ppExpr (Disjunction ann l r) = ppExpr l <> " || " <> ppExpr r
ppExpr (Equality ann l r) = ppExpr l <> " == " <> ppExpr r
ppExpr (Inequality ann l r) = ppExpr l <> " != " <> ppExpr r
ppExpr (LessThan ann l r) = ppExpr l <> " < " <> ppExpr r
ppExpr (LessThanEqual ann l r) = ppExpr l <> " <= " <> ppExpr r
ppExpr (Greater ann l r) = ppExpr l <> " > " <> ppExpr r
ppExpr (GreaterThanEqual ann l r) = ppExpr l <> " >= " <> ppExpr r
ppExpr (Number ann val) = tshow val
ppExpr (Boolean ann True) = "true"
ppExpr (Boolean ann False) = "false"
ppExpr (Character ann val) = "'" <> T.pack [val] <> "'"
ppExpr (String ann val) = "\"" <> val <> "\""
ppExpr (Variable ann name) = name
ppExpr (Call ann callee args) = ppExpr callee <> "(" <> (T.intercalate ", " (map ppExpr args)) <> ")"
ppExpr (ArrayAccess ann array index) = ppExpr array <> "[" <> ppExpr index <> "]"

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
