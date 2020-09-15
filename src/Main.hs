{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Applicative ((<**>), optional)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Text.IO (getContents, readFile)
import Parser (program, pos)
import Prelude hiding (getContents, readFile)
import qualified Options.Applicative as Opt
import Text.Megaparsec (errorBundlePretty, parse)
import System.IO (hPutStr, stderr)
import Semer

default (Text)

data Flags = Flags {
    inputPath :: FilePath,
    outputPath :: Maybe FilePath
}

flagsParser :: Opt.Parser Flags
flagsParser = Flags
    <$> Opt.strArgument (
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
    Flags { inputPath, outputPath } <- Opt.execParser programInfo
    contents <- if inputPath == "-" then getContents else readFile inputPath

    case parse (program pos) inputPath contents of
        Left err -> hPutStr stderr (errorBundlePretty err)
        Right ast -> case typeCheck ast of
            Left errs -> hPutStr stderr (show errs)
            Right () -> print ast
