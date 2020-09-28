{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}

module Codegen where

import Ast

import Control.Monad.State hiding (void)

import Data.Char (ord)
import Data.String (fromString)
import Data.Functor.Identity (runIdentity)
import Data.List (zip4)
import qualified Data.Map as Map
import Data.Text.Lazy.IO as T
import Data.Word (Word32)
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
    -- Pairs variable name with a type and a Value representing a pointer to the stack.
    symbolTable :: [(String, (Ast.Type, Operand))],
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
assign :: String -> Ast.Type -> Operand -> Codegen ()
assign name ty register = do
    table <- gets symbolTable
    modify $ \s -> s { symbolTable = [(name, (ty, register))] ++ table }

-- lifted from sdiehl/kaleidoscope
getvar :: Identifier -> Codegen (Maybe (Ast.Type, Operand))
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
    args' <- mapM (exprCodegen Rval) args
    callee' <- exprCodegen Rval callee -- TODO: possibly should be lval
    call callee' (makeCallArgs args')

{-| Allocate memory on a stack for given type.
When the type is a pointer, we will only allocate a cell of pointer size.
When the type is an array, we will allocate a cell for all `numElements` items
but not recursively – only one dimensional arrays can be currently allocated. -}
alloca_ :: Ast.Type -> Word32 -> Codegen Operand
alloca_ arr@(TArray (Number _ numElements) ty) = alloca (codegenType ty) (Just (ConstantOperand (C.Int 32 (fromIntegral numElements))))
alloca_ arr@(TArray numElementsExpr ty) = \alignment -> do
    numElements <- exprCodegen Rval (mapExpressionAnn (, TBot) numElementsExpr) -- TODO: include annotations in Type
    alloca (codegenType ty) (Just numElements) alignment
alloca_ ty = alloca (codegenType ty) Nothing -- single element

{-| Generate a Value representing constant True. -}
true :: Operand
true = ConstantOperand (C.Int 1 1)

{-| Generate a Value representing constant False. -}
false :: Operand
false = ConstantOperand (C.Int 1 0)

{-| If we use 0 for alignment, LLVM will do its own thing. -}
defaultAlignment = 0

{-| Generate a Value for the result of comparison of two operands of given type for equality. -}
emitIsEqual :: Ast.Type -> Operand -> Operand -> Codegen Operand
emitIsEqual TBool = icmp IntegerPredicate.EQ
emitIsEqual TChar = icmp IntegerPredicate.EQ
emitIsEqual TInt32 = icmp IntegerPredicate.EQ
emitIsEqual TNil = \_ _ -> return true -- Nil tuples are all the same.
emitIsEqual (TPtr _) = icmp IntegerPredicate.EQ -- icmp supports pointer comparisons too.
emitIsEqual (TArray _ _) = icmp IntegerPredicate.EQ -- Arrays are just pointers.

{-| Generate a Value for the result of comparison of two operands of given type for inequality. -}
emitIsNotEqual :: Ast.Type -> Operand -> Operand -> Codegen Operand
emitIsNotEqual TBool = icmp IntegerPredicate.NE
emitIsNotEqual TChar = icmp IntegerPredicate.NE
emitIsNotEqual TInt32 = icmp IntegerPredicate.NE
emitIsNotEqual TNil = \_ _ -> return false -- Nil tuples are all the same.
emitIsNotEqual (TPtr _) = icmp IntegerPredicate.NE
emitIsNotEqual (TArray _ _) = icmp IntegerPredicate.NE

data ValueKind = Lval | Rval deriving (Eq, Show)

exprCodegen :: ValueKind -> Expression (ann, Ast.Type) -> Codegen Operand
exprCodegen vk (Addition ann lhs rhs) = do
    lhs' <- exprCodegen Rval lhs
    rhs' <- exprCodegen Rval rhs
    add lhs' rhs'
exprCodegen vk (Subtraction ann lhs rhs) = do
    lhs' <- exprCodegen Rval lhs
    rhs' <- exprCodegen Rval rhs
    sub lhs' rhs'
exprCodegen vk (Multiplication ann lhs rhs) = do
    lhs' <- exprCodegen Rval lhs
    rhs' <- exprCodegen Rval rhs
    mul lhs' rhs'
exprCodegen vk (Division ann lhs rhs) = do
    lhs' <- exprCodegen Rval lhs
    rhs' <- exprCodegen Rval rhs
    sdiv lhs' rhs'
exprCodegen vk (Conjunction ann lhs rhs) = do
    lhs' <- exprCodegen Rval lhs
    rhs' <- exprCodegen Rval rhs
    Instr.and lhs' rhs'
exprCodegen vk (Disjunction ann lhs rhs) = do
    lhs' <- exprCodegen Rval lhs
    rhs' <- exprCodegen Rval rhs
    Instr.or lhs' rhs'
exprCodegen vk (Negation ann expr) = do
    expr' <- exprCodegen Rval expr
    xor expr' true
exprCodegen vk (Equality (_ann, ty) lhs rhs) = do
    lhs' <- exprCodegen Rval lhs
    rhs' <- exprCodegen Rval rhs
    emitIsEqual ty lhs' rhs'
exprCodegen vk (Inequality (_ann, ty) lhs rhs) = do
    lhs' <- exprCodegen Rval lhs
    rhs' <- exprCodegen Rval rhs
    emitIsNotEqual ty lhs' rhs'
exprCodegen vk (LessThan ty lhs rhs) = do
    lhs' <- exprCodegen Rval lhs
    rhs' <- exprCodegen Rval rhs
    icmp IntegerPredicate.SLT lhs' rhs'
exprCodegen vk (LessThanEqual ty lhs rhs) = do
    lhs' <- exprCodegen Rval lhs
    rhs' <- exprCodegen Rval rhs
    icmp IntegerPredicate.SLE lhs' rhs'
exprCodegen vk (Greater ty lhs rhs) = do
    lhs' <- exprCodegen Rval lhs
    rhs' <- exprCodegen Rval rhs
    icmp IntegerPredicate.SGT lhs' rhs'
exprCodegen vk (GreaterThanEqual ty lhs rhs) = do
    lhs' <- exprCodegen Rval lhs
    rhs' <- exprCodegen Rval rhs
    icmp IntegerPredicate.SGE lhs' rhs'
exprCodegen vk (Number ann n) = return $ BC.int32 (toInteger n)
exprCodegen vk (Boolean ann True) = return $ BC.bit 1
exprCodegen vk (Boolean ann False) = return $ BC.bit 0
exprCodegen vk (Character ann char) = return $ BC.int8 (toInteger (ord char))
exprCodegen vk (String ann text) = do
    -- Let’s create a new global constant and return an its address.
    name <- identifierToName <$> freshName_ ".str"
    constant <- globalStringPtr (T.unpack text) name
    return $ ConstantOperand constant
exprCodegen vk (Variable ann name) = do
    mvar <- getvar name -- Obtain a pointer to memory location of the locally allocated variable.
    case mvar of
        Just (ty, localPointer) -> do
            case (vk, ty) of
                -- If a stored value is an array or a pointer, return it as value.
                (_, TArray _ _) -> return localPointer
                -- Similarly, return the pointer when we are trying to write to it.
                (Lval, _) -> return localPointer
                -- Only when we are trying to read it, return a register with the value of the variable.
                (Rval, _) -> load localPointer defaultAlignment
        Nothing -> do
            gs <- gets globals
            case lookup name gs of
                Nothing -> error $ "Variable not in scope: " ++ show name
                Just ty -> return (ConstantOperand $ C.GlobalReference (codegenType ty) (identifierToName name))

exprCodegen vk (Ast.Call ann callee args) = do
    call_ callee args
exprCodegen vk (Ast.ArrayAccess ann array index) = do
    array' <- exprCodegen Rval array -- Array is already a pointer so no need to use it as lval.
    index' <- exprCodegen Rval index
    indexedValue <- gep array' [index']
    case vk of
        Rval -> load indexedValue defaultAlignment
        Lval -> return indexedValue


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
        cond' <- exprCodegen Rval cond
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
    cond' <- exprCodegen Rval cond
    condBr cond' bodyBlock done
    bodyBlock <- freshBlock "bodyBlock"
    commandsCodegen body
    isTerminated <- hasTerminator
    when (not isTerminated) (br condBlock) -- Each block requires a terminator but we can only have one.
    done <- freshBlock "done"
    return ()
commandCodegen (Return ann expr) = do
    result <- exprCodegen Rval expr
    ret result
commandCodegen (Declaration ann name ty (Just expr)) = do
    val <- exprCodegen Rval expr
    var <- alloca_ ty defaultAlignment
    store var defaultAlignment val
    assign (T.unpack name) ty var
commandCodegen (Declaration ann name ty Nothing) = do
    var <- alloca_ ty defaultAlignment
    assign (T.unpack name) ty var
commandCodegen (Assignment ann lhs rhs) = do
    lhs' <- exprCodegen Lval lhs
    rhs' <- exprCodegen Rval rhs
    store lhs' defaultAlignment rhs'
commandCodegen (CCall ann callee args) = do
    call_ callee args
    return ()

codegenType :: Ast.Type -> AST.Type
codegenType TInt32 = i32
codegenType TChar = i8 -- We will use UTF-8 for simplicity.
codegenType TBool = i1
codegenType TNil = void
codegenType (TPtr nested) = ptr (codegenType nested)
codegenType (TArray (Number _ size) nested) = ArrayType (fromIntegral size) (codegenType nested)
codegenType (TArray _ nested) = ptr (codegenType nested) -- Arrays with dynamic size are just pointers.
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
codegenFunctionDefinition (name, FunctionDefinition _endAnn args resultType variadic mbody) =
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
                        let argTypes = map snd args
                        let argTypes' = map fst args'
                        let argNames = map (T.unpack . fst) args
                        freshBlock "entry"
                        forM (zip4 argNames argTypes argTypes' argOperands) $ \(name, ty, ty', register) -> do
                            var <- alloca ty' Nothing defaultAlignment
                            store var defaultAlignment register
                            assign name ty var
                        commandsCodegen commands
                    )

codegenFunctions :: Program (ann, Ast.Type) -> Modulegen ()
codegenFunctions functions = do
    modify $ \s -> s { globals = map (\(name, def) -> (name, funType def)) functions }
    mapM_ codegenFunctionDefinition functions

codegen :: FilePath -> Program (ann, Ast.Type) -> Module
codegen filename functions = fst (runIdentity (runStateT (runCodegen (buildModuleT (fromString filename) (codegenFunctions functions))) emptyCodegenState))
-- TODO: this is extremely ugly
