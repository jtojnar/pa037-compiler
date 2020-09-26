{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Codegen.LLVM where

import Ast

import Control.Monad.State hiding (void)

import Data.Char (ord)
import Data.String (fromString)
import Data.Functor.Identity (runIdentity)
import qualified Data.Map as Map
import Data.Text.Lazy.IO as T
import qualified Data.Text as T

import LLVM.AST hiding (function)
import LLVM.AST.Global
import LLVM.AST.Linkage
import LLVM.AST.Type as AST
import LLVM.AST.Typed (typeOf)
import qualified LLVM.AST.Constant as C
import LLVM.AST.ParameterAttribute (ParameterAttribute)
import qualified LLVM.AST.IntegerPredicate as IntegerPredicate

import LLVM.IRBuilder.Constant as BC
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction as Instr

data CodegenState = CodegenState {
    -- Pairs variable name with a Value representing a pointer to the stack.
    symbolTable :: [(String, Operand)],
    -- Describes types of global variables.
    -- TODO: This is immutable so it should not really be a part of the state (move to Reader).
    globals :: [(Identifier, Ast.Type)],
    -- Pair variable name with a counter so that we can always have a unique variable name
    names :: Names
} deriving Show

type Names = Map.Map Identifier Int

newtype CodegenInner a = CodegenInner {
    runCodegen :: State CodegenState a
} deriving (Functor, Applicative, Monad, MonadState CodegenState, MonadFix)

-- TODO: I have no idea how this works
type Modulegen = ModuleBuilderT CodegenInner
type Codegen = IRBuilderT Modulegen

emptyCodegenState = CodegenState {
    symbolTable = [],
    globals = [],
    names = Map.empty
}

-- lifted from sdiehl/kaleidoscope
assign :: String -> Operand -> Codegen ()
assign name register = do
    table <- gets symbolTable
    modify $ \s -> s { symbolTable = [(name, register)] ++ table }

-- lifted from sdiehl/kaleidoscope
getvar :: Identifier -> Codegen (Maybe Operand)
getvar var = do
    syms <- gets symbolTable
    return (lookup (T.unpack var) syms)

-- lifted from sdiehl/kaleidoscope
uniqueName :: Identifier -> Names -> (Identifier, Names)
uniqueName nm nms =
    case Map.lookup nm nms of
        Nothing -> (nm, Map.insert nm 1 nms)
        Just ix -> (nm <> "_" <> T.pack (show ix), Map.insert nm (ix+1) nms)

{-| Get an unused variable name for given prefix. -}
freshName_ :: Identifier -> Codegen Identifier
freshName_ prefix = do
    nms <- gets names
    let (qname, supply) = uniqueName prefix nms
    modify $ \s -> s {
        names = supply
    }
    return qname

{-| Generate a fresh labelled block.
Adapted from @LLVM.IRBuilder.Monad.block@ and @named@.
Compared to @named@, it has a benefit of being unique accross functions. -}
freshBlock :: Identifier -> Codegen Name
freshBlock name = do
    nm <- fromString . T.unpack <$> freshName_ name
    emitBlockStart nm
    return nm

{-| Combine arguments into tuples with empty parameter lists. -}
makeCallArgs :: [Operand] -> [(Operand, [ParameterAttribute])]
makeCallArgs args = zip args (repeat [])

{-| Call a function represented by an expression. -}
call_ :: Expression (ann, Ast.Type) -> [Expression (ann, Ast.Type)] -> Codegen Operand
call_ callee args = do
    args' <- mapM exprCodegen args
    callee' <- exprCodegen callee
    call callee' (makeCallArgs args')

{-| Generate a Value representing constant True. -}
true :: Operand
true = ConstantOperand (C.Int 1 1)

{-| Generate a Value representing constant False. -}
false :: Operand
false = ConstantOperand (C.Int 1 0)

{-| Generate a Value for the result of comparison of two operands of given type for equality. -}
emitIsEqual :: Ast.Type -> Operand -> Operand -> Codegen Operand
emitIsEqual TBool = icmp IntegerPredicate.EQ
emitIsEqual TChar = icmp IntegerPredicate.EQ
emitIsEqual TInt32 = icmp IntegerPredicate.EQ
emitIsEqual TNil = \_ _ -> return true -- Nil tuples are all the same.
emitIsEqual (TPtr _) = icmp IntegerPredicate.EQ -- icmp supports pointer comparisons too.

{-| Generate a Value for the result of comparison of two operands of given type for inequality. -}
emitIsNotEqual :: Ast.Type -> Operand -> Operand -> Codegen Operand
emitIsNotEqual TBool = icmp IntegerPredicate.NE
emitIsNotEqual TChar = icmp IntegerPredicate.NE
emitIsNotEqual TInt32 = icmp IntegerPredicate.NE
emitIsNotEqual TNil = \_ _ -> return false -- Nil tuples are all the same.
emitIsNotEqual (TPtr _) = icmp IntegerPredicate.NE

exprCodegen :: Expression (ann, Ast.Type) -> Codegen Operand
exprCodegen (Addition ann lhs rhs) = do
    lhs' <- exprCodegen lhs
    rhs' <- exprCodegen rhs
    add lhs' rhs'
exprCodegen (Subtraction ann lhs rhs) = do
    lhs' <- exprCodegen lhs
    rhs' <- exprCodegen rhs
    sub lhs' rhs'
exprCodegen (Multiplication ann lhs rhs) = do
    lhs' <- exprCodegen lhs
    rhs' <- exprCodegen rhs
    mul lhs' rhs'
exprCodegen (Division ann lhs rhs) = do
    lhs' <- exprCodegen lhs
    rhs' <- exprCodegen rhs
    sdiv lhs' rhs'
exprCodegen (Conjunction ann lhs rhs) = do
    lhs' <- exprCodegen lhs
    rhs' <- exprCodegen rhs
    Instr.and lhs' rhs'
exprCodegen (Disjunction ann lhs rhs) = do
    lhs' <- exprCodegen lhs
    rhs' <- exprCodegen rhs
    Instr.or lhs' rhs'
exprCodegen (Negation ann expr) = do
    expr' <- exprCodegen expr
    xor expr' true
exprCodegen (Equality (_ann, ty) lhs rhs) = do
    lhs' <- exprCodegen lhs
    rhs' <- exprCodegen rhs
    emitIsEqual ty lhs' rhs'
exprCodegen (Inequality (_ann, ty) lhs rhs) = do
    lhs' <- exprCodegen lhs
    rhs' <- exprCodegen rhs
    emitIsNotEqual ty lhs' rhs'
exprCodegen (LessThan ty lhs rhs) = do
    lhs' <- exprCodegen lhs
    rhs' <- exprCodegen rhs
    icmp IntegerPredicate.SLT lhs' rhs'
exprCodegen (LessThanEqual ty lhs rhs) = do
    lhs' <- exprCodegen lhs
    rhs' <- exprCodegen rhs
    icmp IntegerPredicate.SLE lhs' rhs'
exprCodegen (Greater ty lhs rhs) = do
    lhs' <- exprCodegen lhs
    rhs' <- exprCodegen rhs
    icmp IntegerPredicate.SGT lhs' rhs'
exprCodegen (GreaterThanEqual ty lhs rhs) = do
    lhs' <- exprCodegen lhs
    rhs' <- exprCodegen rhs
    icmp IntegerPredicate.SGE lhs' rhs'
exprCodegen (Number ann n) = return $ BC.int32 (toInteger n)
exprCodegen (Boolean ann True) = return $ BC.bit 1
exprCodegen (Boolean ann False) = return $ BC.bit 0
exprCodegen (Character ann char) = return $ BC.int8 (toInteger (ord char))
exprCodegen (String ann text) = do
    -- Let’s create a new global constant and return an its address.
    -- https://stackoverflow.com/questions/1061753/how-can-i-implement-a-string-data-type-in-llvm
    name <- freshName_ ".str"

    let members = ((map (C.Int 8 . toInteger . ord) . T.unpack) text)
    let array = C.Array (typeOf (head members)) members
    let ty = typeOf array
    var <- internalGlobal (identifierToName name) ty array
    gep var [ConstantOperand (C.Int 32 0), ConstantOperand (C.Int 64 0)]
  where
    -- Based on llvm-hs’s global.
    internalGlobal nm ty initVal = do
        emitDefn $ GlobalDefinition globalVariableDefaults {
            name = nm,
            LLVM.AST.Global.type' = ty,
            linkage = Internal,
            isConstant = True,
            initializer = Just initVal
        }
        pure $ ConstantOperand $ C.GlobalReference (ptr ty) nm
exprCodegen (Variable ann name) = do
    mLocalPointer <- getvar name -- Obtain a pointer to memory location of the locally allocated variable.
    case mLocalPointer of
        Just localPointer -> do
            let alignment = 0 -- default alignment
            load localPointer alignment -- Return the register with the value of the variable.
        Nothing -> do
            gs <- gets globals
            case lookup name gs of
                Nothing -> error $ "Variable not in scope: " ++ show name
                Just ty -> return (ConstantOperand $ C.GlobalReference (codegenType ty) (identifierToName name))

exprCodegen (Ast.Call ann callee args) = do
    call_ callee args

-- FIXME: handle commands dependently
{-| Generate code for sequence of commands. -}
commandsCodegen :: [Command (ann, Ast.Type)] -> Codegen ()
commandsCodegen commands = mapM_ commandCodegen commands

{-| Generate a code for a single command. -}
commandCodegen :: Command (ann, Ast.Type) -> Codegen ()
commandCodegen (Conditional ann ifBranches melse) = mdo
    br condBlock
    condBlock <- codegenBranches done ifBranches
    ifFalse <- case melse of
        Just elseBranch -> do
            ifFalse <- freshBlock "ifFalse"
            commandsCodegen elseBranch
            isTerminated <- hasTerminator
            when (not isTerminated) (br done) -- Each block requires a terminator but we can only have one.
            return ifFalse
        Nothing -> do
            return done -- There is no else branch so we just jump to done if false.
    done <- freshBlock "done"
    return ()
  where
    -- Generate a code for a single “if” or “elseif” branch.
    codegenBranch :: Expression (ann, Ast.Type) -> Commands (ann, Ast.Type) -> Name -> Name -> Codegen Name
    codegenBranch cond ifBranch done ifFalse = mdo
        condBlock <- freshBlock "condBlock"
        cond' <- exprCodegen cond
        condBr cond' ifTrue ifFalse
        ifTrue <- freshBlock "ifTrue"
        commandsCodegen ifBranch
        isTerminated <- hasTerminator
        when (not isTerminated) (br done) -- Each block requires a terminator but we can only have one.
        return condBlock

    -- Generate a code for all “if” and “elseif” branches.
    -- It generates code for a branch and then recursively handles the remaining branches,
    -- using MonadFix to route the block containing the condition to the branch instruction
    -- of the preceding branch.
    -- TODO: Figure out why we cannot just use mfix with mapM. Not lazy enough?
    codegenBranches :: Name -> [(Expression (ann, Ast.Type), Commands (ann, Ast.Type))] -> Codegen Name
    codegenBranches done [(cond, branch)] = codegenBranch cond branch done done
    codegenBranches done ((cond, branch):restBranches) = mdo
        condBlock <- codegenBranch cond branch done nextCondBlock
        nextCondBlock <- codegenBranches done restBranches
        return condBlock
-- commandCodegen (ForEach ann item collection body) = mdo
-- FIXME: implement foreach
commandCodegen (While ann cond body) = mdo
    br condBlock
    condBlock <- freshBlock "condBlock"
    cond' <- exprCodegen cond
    condBr cond' bodyBlock done
    bodyBlock <- freshBlock "bodyBlock"
    commandsCodegen body
    isTerminated <- hasTerminator
    when (not isTerminated) (br condBlock) -- Each block requires a terminator but we can only have one.
    done <- freshBlock "done"
    return ()
commandCodegen (Return ann expr) = do
    result <- exprCodegen expr
    ret result
commandCodegen (Declaration ann name ty (Just expr)) = do
    val <- exprCodegen expr
    var <- alloca (codegenType ty) Nothing 0 -- default number of elements and alignment
    store var 0 val -- default alignment
    assign (T.unpack name) var
commandCodegen (Declaration ann name ty Nothing) = do
    var <- alloca (codegenType ty) Nothing 0 -- default number of elements and alignment
    assign (T.unpack name) var
commandCodegen (Assignment ann name expr) = do
    val <- exprCodegen expr
    var <- getvar name
    case var of
        Nothing -> error $ "Variable not in scope: " ++ show name
        Just var' -> store var' 0 val -- default alignment
commandCodegen (CCall ann callee args) = do
    call_ callee args
    return ()

codegenType :: Ast.Type -> AST.Type
codegenType TInt32 = i32
codegenType TChar = i8 -- We will use UTF-8 for simplicity.
codegenType TBool = i1
codegenType TNil = void
codegenType (TPtr nested) = ptr (codegenType nested)
codegenType (Ast.Function args result variadic) =
    FunctionType {
        resultType = codegenType result,
        argumentTypes = map codegenType args,
        isVarArg = variadic
    }

identifierToName :: Identifier -> Name
identifierToName = mkName . T.unpack

identifierToParameterName :: Identifier -> ParameterName
identifierToParameterName = fromString . T.unpack

codegenFunctionDefinition :: (Identifier, FunctionDefinition (ann, Ast.Type)) -> Modulegen Operand
codegenFunctionDefinition (name, FunctionDefinition args resultType variadic mbody) =
    let
        name' = identifierToName name
        args' = map (\(name, ty) -> (codegenType ty, identifierToParameterName name)) args
        resultType' = codegenType resultType
    in do
        case mbody of
            Nothing ->
                (if variadic then externVarArgs else extern) name' (map fst args') resultType'
            Just commands ->
                function
                    name'
                    args'
                    resultType'
                    (\argOperands -> do
                        let argTypes = map fst args'
                        let argNames = map (T.unpack . fst) args
                        freshBlock "entry"
                        forM (zip3 argNames argTypes argOperands) $ \(name, ty, register) -> do
                            var <- alloca ty Nothing 0 -- default number of elements and alignment
                            store var 0 register -- default alignment
                            assign name var
                        commandsCodegen commands
                    )

codegenFunctions :: Program (ann, Ast.Type) -> Modulegen ()
codegenFunctions functions = do
    modify $ \s -> s { globals = map (\(name, def) -> (name, funType def)) functions }
    mapM_ codegenFunctionDefinition functions

codegen :: FilePath -> Program (ann, Ast.Type) -> Module
codegen filename functions = fst (runIdentity (runStateT (runCodegen (buildModuleT (fromString filename) (codegenFunctions functions))) emptyCodegenState))
-- TODO: this is extremely ugly
