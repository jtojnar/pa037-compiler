{-# LANGUAGE OverloadedStrings #-}

module Printer (ppType) where

import Ast
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T

default (Text)

ppType :: Type -> Text
ppType TBot = "⊥"
ppType TInt32 = "i32"
ppType TChar = "char"
ppType TBool = "bool"
ppType TNil = "()"
ppType (TPtr t) = "ptr " <> ppType t
ppType (Function args result variadic) = "fn(" <> T.intercalate ", " (map ppType args) <> (if variadic then ", …" else "") <> ") -> " <> ppType result
