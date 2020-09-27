{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Ast (Program, Type)
import Codegen.LLVM (codegen)
import Control.Applicative ((<|>), (<**>), optional)
import Control.Monad (void)
import Data.Semigroup ((<>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.IO (getContents, readFile)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Helpers
import LLVM.Pretty
import Parser (Pos, program, pos)
import Prelude hiding (getContents, readFile)
import qualified Options.Applicative as Opt
import Text.Megaparsec (errorBundlePretty, parse)
import System.IO (Handle, IOMode(..), hPutStr, withFile, stderr, stdout)
import System.FilePath ((-<.>), dropExtension, takeFileName)
import System.Process
import Semer

default (Text)

data BuildStage = PrintAst | CheckAst | EmitIr | ConvertToAssembly | Compile deriving (Eq, Ord)

data Flags = Flags {
    finalStage :: BuildStage,
    inputPath :: FilePath,
    mOutputPath :: Maybe FilePath
}

modeFlags :: Opt.Parser BuildStage
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
        <> Opt.help "Generate LLVM intermediate representation"
    )
    <|> Opt.flag' ConvertToAssembly (
        Opt.long "convert-to-assembly"
        <> Opt.help "Generate assembly for current system"
    )
    <|> Opt.flag' Compile (
        Opt.long "convert-to-assembly"
        <> Opt.help "Generate assembly for current system (default action)"
    )
    <|> pure Compile

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

defaultOutputPath :: BuildStage -> FilePath -> FilePath
defaultOutputPath PrintAst _ = "-"
defaultOutputPath CheckAst _ = "-"
defaultOutputPath EmitIr "-" = "stdin.ll"
defaultOutputPath EmitIr inputPath = takeFileName inputPath -<.> "ll"
defaultOutputPath ConvertToAssembly inputPath = "stdin.s"
defaultOutputPath ConvertToAssembly inputPath = takeFileName inputPath -<.> "s"
defaultOutputPath Compile inputPath = "a.out"
defaultOutputPath Compile inputPath = dropExtension (takeFileName inputPath)

mkOutputHandler :: FilePath -> (Handle -> IO ()) -> IO ()
mkOutputHandler "-" = \fn -> fn stdout
mkOutputHandler path = withFile path WriteMode

main :: IO ()
main = do
    flags@(Flags { finalStage, inputPath, mOutputPath }) <- Opt.execParser programInfo
    contents <- if inputPath == "-" then getContents else readFile inputPath
    let outputPath = fromMaybe (defaultOutputPath finalStage inputPath) mOutputPath

    mkOutputHandler outputPath $ \output -> case parse (program pos) inputPath contents of
        Left err -> hPutStr stderr (errorBundlePretty err)
        Right ast -> handleAst output outputPath ast flags

handleAst :: Handle -> FilePath -> Program Pos -> Flags -> IO ()
handleAst output outputPath ast Flags { finalStage = PrintAst } = T.hPutStrLn output (T.pack (show ast))
handleAst output outputPath ast flags =
    case typeCheck ast of
        ([], ast') -> handleTypeCheckedAst output outputPath ast' flags
        (errs, ast) -> hPutStr stderr (show errs)

handleTypeCheckedAst :: Handle -> FilePath -> Program (Pos, Type) -> Flags -> IO ()
handleTypeCheckedAst output outputPath ast Flags { finalStage = CheckAst } = T.hPutStrLn output (T.pack (show ast))
handleTypeCheckedAst output outputPath ast flags@(Flags { inputPath }) = handleLlvmIr output outputPath (ppllvm (codegen inputPath ast)) flags

handleLlvmIr :: Handle -> FilePath -> T.Text -> Flags -> IO ()
handleLlvmIr output outputPath ir Flags { finalStage = EmitIr } = T.hPutStrLn output ir
handleLlvmIr output outputPath ir flags = do
    assembly <- readProcess "llc" ["-o", "-"] (T.unpack ir)
    handleAssembly output outputPath assembly flags

handleAssembly :: Handle -> FilePath -> String -> Flags -> IO ()
handleAssembly output outputPath assembly Flags { finalStage = ConvertToAssembly } = hPutStr output assembly
handleAssembly output outputPath assembly flags = do
    void (readProcess "clang" ["-x", "assembler", "-", "-o", outputPath] assembly)
