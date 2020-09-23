{-# LANGUAGE OverloadedStrings #-}

module Printer (ppType) where

import Ast
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T

default (Text)

ppType :: Type -> Text
ppType TBot = "⊥"
ppType TInt32 = "int32"
ppType TChar = "char"
ppType TBool = "bool"
ppType TNil = "()"
ppType (TPtr t) = "ptr " <> ppType t
ppType (Function args result) = "fn(" <> T.intercalate ", " (map ppType args) <> ") -> " <> ppType result
