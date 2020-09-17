{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Codegen.LLVM where

import Ast

import Data.Text.Lazy.IO as T

import LLVM.Pretty
import LLVM.AST hiding (function)
import LLVM.AST.Type as AST
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.Constant as C

import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction

--exprCodegen (Addition ann lhs rhs)
--exprCodegen (Subtraction ann lhs rhs)
--exprCodegen (Multiplication ann lhs rhs)
--exprCodegen (Division ann lhs rhs)
--exprCodegen (Conjunction ann lhs rhs)
--exprCodegen (Disjunction ann lhs rhs)
--exprCodegen (Negation ann (Expression ann))
--exprCodegen (Equality ann lhs rhs)
--exprCodegen (Inequality ann lhs rhs)
--exprCodegen (LessThan ann lhs rhs)
--exprCodegen (LessThanEqual ann lhs rhs)
--exprCodegen (Greater ann lhs rhs)
--exprCodegen (GreaterThanEqual ann lhs rhs)
exprCodegen (Number ann n) = ConstantFP::get(TheContext, APFloat(n))
--exprCodegen (Boolean ann Bool)
--exprCodegen (Character ann Char)
--exprCodegen (String ann Text)
--exprCodegen (Variable ann Identifier)
--exprCodegen (Call ann Identifier [(Expression ann)])

simple :: IO ()
simple = T.putStrLn $ ppllvm $ buildModule "exampleModule" $ mdo
  function "add" [(i32, "a"), (i32, "b")] i32 $ \[a, b] -> mdo

    entry <- block `named` "entry"; do
      c <- add a b
      ret c
