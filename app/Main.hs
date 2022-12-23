module Main where

import System.Environment (getArgs)
import qualified Data.Text.IO as T
import Parser (parseProgram)
import Printer (printProgram, printParseResult)
import Interpreter ( interpretProgram )
import Text.Megaparsec (errorBundlePretty)
import Text.Megaparsec.Error (ParseErrorBundle)

main :: IO ()
main = do
  [inputFilePath] <- getArgs
  inputFile <- T.readFile inputFilePath
  let parseResult = parseProgram inputFilePath inputFile
  case parseResult of
    Left e -> putStrLn $ errorBundlePretty e
    Right p -> putStrLn $ printProgram (interpretProgram p)
    