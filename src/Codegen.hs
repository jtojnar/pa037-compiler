{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}

module Codegen where

import Ast

import Control.Monad.State hiding (void)
import Control.Monad (void)

import Data.Char (ord)
import Data.String (fromString)
import Data.Functor.Identity (runIdentity)
import Data.List (find, zip4)
import Data.Maybe (isJust)
import qualified Data.Map as Map
import Data.Word (Word32)
import qualified Data.Text as T

import LLVM.AST (Name, Module, Operand(..))
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import LLVM.AST.ParameterAttribute (ParameterAttribute)
import qualified LLVM.AST.IntegerPredicate as IntegerPredicate
import LLVM.AST.Type (Type(..))
import qualified LLVM.AST.Type as Type

import qualified LLVM.IRBuilder.Constant as BC
import LLVM.IRBuilder.Module (ModuleBuilderT, ParameterName, buildModuleT, extern, externVarArgs, function)
import LLVM.IRBuilder.Monad (IRBuilderT, emitBlockStart, hasTerminator)
import qualified LLVM.IRBuilder.Instruction as Instr

data CodegenState = CodegenState {
    -- Pairs variable name with a type and a Value representing a pointer to the stack.
    -- Each lexical block has its own symbol table and can access symbols defined in parent scopes if not shadowed.
    -- Top to bottom ~ left to right.
    symbolTables :: [[(String, (Ast.Type, Operand))]],
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

emptyCodegenState :: CodegenState
emptyCodegenState = CodegenState {
    symbolTables = [[]],
    globals = [],
    names = Map.empty
}

{-| Register a variable into a symbol table for current block. -}
assign :: String -> Ast.Type -> Operand -> Codegen ()
assign name ty register = do
    modify $ \s -> s {
        symbolTables =
            let
                topFrame : rest = symbolTables s
            in
                ((name, (ty, register)) : topFrame) : rest
    }

{-| Get a Value representing the stack memory location where given variable is stored.
Descend down the stack of local block scopes when not found in the top level one. -}
getvar :: Identifier -> Codegen (Maybe (Ast.Type, Operand))
getvar var = do
    syms <- gets symbolTables
    return (join (find isJust (map (lookup (T.unpack var)) syms)))

{-| Create a new symbol table for the current block and add it to the stack. -}
enterBlock :: Codegen ()
enterBlock = do
    modify $ \s -> s { symbolTables = [] : symbolTables s }

{-| Remove the symbol table for the current block from the stack. -}
exitBlock :: Codegen ()
exitBlock = do
    modify $ \s -> s { symbolTables = tail (symbolTables s) }

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
    Instr.call callee' (makeCallArgs args')

{-| Allocate memory on a stack for given type.
When the type is a pointer, we will only allocate a cell of pointer size.
When the type is an array, we will allocate a cell for all `numElements` items
but not recursively – only one dimensional arrays can be currently allocated. -}
alloca_ :: Ast.Type -> Word32 -> Codegen Operand
alloca_ (TArray (Number _ numElements) ty) = Instr.alloca (codegenType ty) (Just (ConstantOperand (C.Int 32 (fromIntegral numElements))))
alloca_ (TArray numElementsExpr ty) = \alignment -> do
    numElements <- exprCodegen Rval (mapExpressionAnn (, TBot) numElementsExpr) -- TODO: include annotations in Type
    Instr.alloca (codegenType ty) (Just numElements) alignment
alloca_ ty = Instr.alloca (codegenType ty) Nothing -- single element

{-| Generate a Value representing constant True. -}
true :: Operand
true = ConstantOperand (C.Int 1 1)

{-| Generate a Value representing constant False. -}
false :: Operand
false = ConstantOperand (C.Int 1 0)

{-| If we use 0 for alignment, LLVM will do its own thing. -}
defaultAlignment :: Word32
defaultAlignment = 0

{-| Generate a Value for the result of comparison of two operands of given type for equality. -}
emitIsEqual :: Ast.Type -> Operand -> Operand -> Codegen Operand
emitIsEqual TBool = Instr.icmp IntegerPredicate.EQ
emitIsEqual TChar = Instr.icmp IntegerPredicate.EQ
emitIsEqual TInt32 = Instr.icmp IntegerPredicate.EQ
emitIsEqual (TPtr _) = Instr.icmp IntegerPredicate.EQ -- icmp supports pointer comparisons too.
emitIsEqual (TArray _ _) = Instr.icmp IntegerPredicate.EQ -- Arrays are just pointers.
emitIsEqual TBot = error "Bottoms should not be compared"
emitIsEqual (Function _ _ _) = error "Functions should not be compared"
emitIsEqual (TProduct _) = error "Product types should not be compared"

{-| Generate a Value for the result of comparison of two operands of given type for inequality. -}
emitIsNotEqual :: Ast.Type -> Operand -> Operand -> Codegen Operand
emitIsNotEqual TBool = Instr.icmp IntegerPredicate.NE
emitIsNotEqual TChar = Instr.icmp IntegerPredicate.NE
emitIsNotEqual TInt32 = Instr.icmp IntegerPredicate.NE
emitIsNotEqual (TPtr _) = Instr.icmp IntegerPredicate.NE
emitIsNotEqual (TArray _ _) = Instr.icmp IntegerPredicate.NE
emitIsNotEqual TBot = error "Bottoms should not be compared"
emitIsNotEqual (Function _ _ _) = error "Functions should not be compared"
emitIsNotEqual (TProduct _) = error "Product types should not be compared"

data ValueKind = Lval | Rval deriving (Eq, Show)

exprCodegen :: ValueKind -> Expression (ann, Ast.Type) -> Codegen Operand
exprCodegen _vk (Addition _ann lhs rhs) = do
    lhs' <- exprCodegen Rval lhs
    rhs' <- exprCodegen Rval rhs
    Instr.add lhs' rhs'
exprCodegen _vk (Subtraction _ann lhs rhs) = do
    lhs' <- exprCodegen Rval lhs
    rhs' <- exprCodegen Rval rhs
    Instr.sub lhs' rhs'
exprCodegen _vk (Multiplication _ann lhs rhs) = do
    lhs' <- exprCodegen Rval lhs
    rhs' <- exprCodegen Rval rhs
    Instr.mul lhs' rhs'
exprCodegen _vk (Division _ann lhs rhs) = do
    lhs' <- exprCodegen Rval lhs
    rhs' <- exprCodegen Rval rhs
    Instr.sdiv lhs' rhs'
exprCodegen _vk (Conjunction _ann lhs rhs) = do
    lhs' <- exprCodegen Rval lhs
    rhs' <- exprCodegen Rval rhs
    Instr.and lhs' rhs'
exprCodegen _vk (Disjunction _ann lhs rhs) = do
    lhs' <- exprCodegen Rval lhs
    rhs' <- exprCodegen Rval rhs
    Instr.or lhs' rhs'
exprCodegen _vk (Negation _ann expr) = do
    expr' <- exprCodegen Rval expr
    Instr.xor expr' true
exprCodegen _vk (Equality (_ann, ty) lhs rhs) = do
    lhs' <- exprCodegen Rval lhs
    rhs' <- exprCodegen Rval rhs
    emitIsEqual ty lhs' rhs'
exprCodegen _vk (Inequality (_ann, ty) lhs rhs) = do
    lhs' <- exprCodegen Rval lhs
    rhs' <- exprCodegen Rval rhs
    emitIsNotEqual ty lhs' rhs'
exprCodegen _vk (LessThan _ann lhs rhs) = do
    lhs' <- exprCodegen Rval lhs
    rhs' <- exprCodegen Rval rhs
    Instr.icmp IntegerPredicate.SLT lhs' rhs'
exprCodegen _vk (LessThanEqual _ann lhs rhs) = do
    lhs' <- exprCodegen Rval lhs
    rhs' <- exprCodegen Rval rhs
    Instr.icmp IntegerPredicate.SLE lhs' rhs'
exprCodegen _vk (Greater _ann lhs rhs) = do
    lhs' <- exprCodegen Rval lhs
    rhs' <- exprCodegen Rval rhs
    Instr.icmp IntegerPredicate.SGT lhs' rhs'
exprCodegen _vk (GreaterThanEqual _ann lhs rhs) = do
    lhs' <- exprCodegen Rval lhs
    rhs' <- exprCodegen Rval rhs
    Instr.icmp IntegerPredicate.SGE lhs' rhs'
exprCodegen _vk (Number _ann n) = return $ BC.int32 (toInteger n)
exprCodegen _vk (Boolean _ann True) = return $ BC.bit 1
exprCodegen _vk (Boolean _ann False) = return $ BC.bit 0
exprCodegen _vk (Character _ann char) = return $ BC.int8 (toInteger (ord char))
exprCodegen _vk (String _ann text) = do
    -- Let’s create a new global constant and return an its address.
    name <- identifierToName <$> freshName_ ".str"
    constant <- Instr.globalStringPtr (T.unpack text) name
    return $ ConstantOperand constant
exprCodegen vk (Variable _ann name) = do
    mvar <- getvar name -- Obtain a pointer to memory location of the locally allocated variable.
    case mvar of
        Just (ty, localPointer) -> do
            case (vk, ty) of
                -- If a stored value is an array or a pointer, return it as value.
                (_, TArray _ _) -> return localPointer
                -- Similarly, return the pointer when we are trying to write to it.
                (Lval, _) -> return localPointer
                -- Only when we are trying to read it, return a register with the value of the variable.
                (Rval, _) -> Instr.load localPointer defaultAlignment
        Nothing -> do
            gs <- gets globals
            case lookup name gs of
                Nothing -> error $ "Variable not in scope: " ++ show name
                Just ty -> return (ConstantOperand $ C.GlobalReference (codegenType ty) (identifierToName name))

exprCodegen _vk (Ast.Call _ann callee args) = do
    call_ callee args
exprCodegen vk (Ast.ArrayAccess _ann array index) = do
    array' <- exprCodegen Rval array -- Array is already a pointer so no need to use it as lval.
    index' <- exprCodegen Rval index
    indexedValue <- Instr.gep array' [index']
    case vk of
        Rval -> Instr.load indexedValue defaultAlignment
        Lval -> return indexedValue
exprCodegen vk (Ast.ProductFieldAccess _ann prod index) = do
    prod' <- exprCodegen Lval prod
    let index' = BC.int32 (read (T.unpack index))
    indexedValue <- Instr.gep prod' [BC.int32 0, index']
    case vk of
        Rval -> Instr.load indexedValue defaultAlignment
        Lval -> return indexedValue
exprCodegen _vk (Ast.AddressOf _ann e) = exprCodegen Lval e


-- FIXME: handle commands dependently
{-| Generate code for sequence of commands. -}
commandsCodegen :: [Command (ann, Ast.Type)] -> Codegen ()
commandsCodegen commands = do
    enterBlock
    mapM_ commandCodegen commands
    exitBlock

{-| Generate a code for a single command. -}
commandCodegen :: Command (ann, Ast.Type) -> Codegen ()
commandCodegen (Conditional _ann ifBranches melse) = mdo
    Instr.br condBlock
    condBlock <- codegenBranches done finalBlock ifBranches
    finalBlock <- case melse of
        Just elseBranch -> do
            ifFalse <- freshBlock "ifFalse"
            commandsCodegen elseBranch
            isTerminated <- hasTerminator
            when (not isTerminated) (Instr.br done) -- Each block requires a terminator but we can only have one.
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
        Instr.condBr cond' ifTrue ifFalse
        ifTrue <- freshBlock "ifTrue"
        commandsCodegen ifBranch
        isTerminated <- hasTerminator
        when (not isTerminated) (Instr.br done) -- Each block requires a terminator but we can only have one.
        return condBlock

    -- Generate a code for all “if” and “elseif” branches.
    -- It generates code for a branch and then recursively handles the remaining branches,
    -- using MonadFix to route the block containing the condition to the branch instruction
    -- of the preceding branch.
    -- TODO: Figure out why we cannot just use mfix with mapM. Not lazy enough?
    codegenBranches :: Name -> Name -> [(Expression (ann, Ast.Type), Commands (ann, Ast.Type))] -> Codegen Name
    codegenBranches _done finalBlock [] = return finalBlock
    codegenBranches done finalBlock ((cond, branch):restBranches) = mdo
        condBlock <- codegenBranch cond branch done nextCondBlock
        nextCondBlock <- codegenBranches done finalBlock restBranches
        return condBlock
commandCodegen (While _ann cond body) = mdo
    Instr.br condBlock
    condBlock <- freshBlock "condBlock"
    cond' <- exprCodegen Rval cond
    Instr.condBr cond' bodyBlock done
    bodyBlock <- freshBlock "bodyBlock"
    commandsCodegen body
    isTerminated <- hasTerminator
    when (not isTerminated) (Instr.br condBlock) -- Each block requires a terminator but we can only have one.
    done <- freshBlock "done"
    return ()
commandCodegen (Return _ann expr) = do
    result <- exprCodegen Rval expr
    Instr.ret result
commandCodegen (Declaration _ann name ty (Just expr)) = do
    val <- exprCodegen Rval expr
    var <- alloca_ ty defaultAlignment
    Instr.store var defaultAlignment val
    assign (T.unpack name) ty var
commandCodegen (Declaration _ann name ty Nothing) = do
    var <- alloca_ ty defaultAlignment
    assign (T.unpack name) ty var
commandCodegen (Assignment _ann lhs rhs) = do
    lhs' <- exprCodegen Lval lhs
    rhs' <- exprCodegen Rval rhs
    Instr.store lhs' defaultAlignment rhs'
commandCodegen (CCall _ann callee args) = do
    void (call_ callee args)
    return ()

codegenType :: Ast.Type -> AST.Type
codegenType TInt32 = Type.i32
codegenType TChar = Type.i8 -- We will use UTF-8 for simplicity.
codegenType TBool = Type.i1
codegenType (TPtr nested) = Type.ptr (codegenType nested)
codegenType (TArray (Number _ size) nested) = ArrayType (fromIntegral size) (codegenType nested)
codegenType (TArray _ nested) = Type.ptr (codegenType nested) -- Arrays with dynamic size are just pointers.
codegenType (Ast.Function args result variadic) =
    FunctionType {
        resultType = codegenType result,
        argumentTypes = map codegenType args,
        isVarArg = variadic
    }
codegenType (TProduct []) = Type.void
codegenType (TProduct fields) = Type.StructureType False (map (codegenType . snd) fields)
codegenType TBot = error "Cannot realize bottom type."

identifierToName :: Identifier -> Name
identifierToName = AST.mkName . T.unpack

identifierToParameterName :: Identifier -> ParameterName
identifierToParameterName = fromString . T.unpack

codegenFunctionDefinition :: (Identifier, FunctionDefinition (ann, Ast.Type)) -> Modulegen Operand
codegenFunctionDefinition (name, FunctionDefinition _endAnn args returnType variadic mbody) =
    let
        name' = identifierToName name
        args' = map (\(argName, ty) -> (codegenType ty, identifierToParameterName argName)) args
        returnType' = codegenType returnType
    in do
        case mbody of
            Nothing ->
                (if variadic then externVarArgs else extern) name' (map fst args') returnType'
            Just commands ->
                function
                    name'
                    args'
                    returnType'
                    (\argOperands -> do
                        let argTypes = map snd args
                        let argTypes' = map fst args'
                        let argNames = map (T.unpack . fst) args
                        void (freshBlock "entry")
                        forM_ (zip4 argNames argTypes argTypes' argOperands) $ \(argName, ty, ty', register) -> do
                            var <- Instr.alloca ty' Nothing defaultAlignment
                            Instr.store var defaultAlignment register
                            assign argName ty var
                        commandsCodegen commands
                    )

codegenFunctions :: Program (ann, Ast.Type) -> Modulegen ()
codegenFunctions functions = do
    modify $ \s -> s { globals = map (\(name, def) -> (name, funType def)) functions }
    mapM_ codegenFunctionDefinition functions

codegen :: FilePath -> Program (ann, Ast.Type) -> Module
codegen filename functions = fst (runIdentity (runStateT (runCodegen (buildModuleT (fromString filename) (codegenFunctions functions))) emptyCodegenState))
-- TODO: this is extremely ugly
