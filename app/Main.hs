module Main where

import System.Environment (getArgs)
import qualified Data.Text.IO as T
import Parser (parseProgram)

import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = do
  [inputFilePath] <- getArgs
  inputFile <- T.readFile inputFilePath
  let parseResult = parseProgram inputFilePath inputFile
  case parseResult of
    Left e -> putStrLn $ errorBundlePretty e
    Right r -> mapM_ (print . show) r -- putStrLn $ show r
  return ()