{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Ast (Program, Type)
import Codegen.LLVM (codegen)
import Control.Applicative ((<|>), (<**>), optional)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Text.IO (getContents, readFile)
import qualified Data.Text.Lazy.IO as T
import LLVM.Pretty
import Parser (Pos, program, pos)
import Prelude hiding (getContents, readFile)
import qualified Options.Applicative as Opt
import Text.Megaparsec (errorBundlePretty, parse)
import System.IO (hPutStr, stderr)
import Semer

default (Text)

data CompilerMode = PrintAst | CheckAst | EmitIr

data Flags = Flags {
    mode :: CompilerMode,
    inputPath :: FilePath,
    outputPath :: Maybe FilePath
}

modeFlags :: Opt.Parser CompilerMode
modeFlags =
    Opt.flag' PrintAst (
        Opt.long "print-ast"
        <> Opt.help "Print parsed abstract syntax tree"
    )
    <|> Opt.flag' CheckAst (
        Opt.long "check-ast"
        <> Opt.help "Print parsed and type checked abstract syntax tree"
    )
    <|> Opt.flag' EmitIr (
        Opt.long "emit-ir"
        <> Opt.help "Print LLVM intermediate representation (default action)"
    )
    <|> pure EmitIr

flagsParser :: Opt.Parser Flags
flagsParser = Flags
    <$> modeFlags
    <*> Opt.strArgument (
        Opt.metavar "SOURCE"
        <> Opt.help "Path to a file with the source code to be compiled"
    )
    <*> (optional . Opt.strOption) (
        Opt.short 'o'
        <> Opt.metavar "OUTPUT"
        <> Opt.help "Path for the generated executable file"
    )

programInfo :: Opt.ParserInfo Flags
programInfo =
    Opt.info
        (flagsParser <**> Opt.helper)
        (Opt.fullDesc <> Opt.progDesc "Simple compiler for PA037 course" <> Opt.header "compiler")

main :: IO ()
main = do
    flags@(Flags { mode, inputPath, outputPath }) <- Opt.execParser programInfo
    contents <- if inputPath == "-" then getContents else readFile inputPath

    case parse (program pos) inputPath contents of
        Left err -> hPutStr stderr (errorBundlePretty err)
        Right ast -> handleAst ast flags

handleAst :: Program Pos -> Flags -> IO ()
handleAst ast Flags { mode = PrintAst } = print ast
handleAst ast flags =
    case typeCheck ast of
        ([], ast') -> handleTypeCheckedAst ast' flags
        (errs, ast) -> hPutStr stderr (show errs)

handleTypeCheckedAst :: Program (Pos, Type) -> Flags -> IO ()
handleTypeCheckedAst ast Flags { mode = CheckAst } = print ast
handleTypeCheckedAst ast Flags { inputPath } = T.putStrLn $ ppllvm (codegen inputPath ast)