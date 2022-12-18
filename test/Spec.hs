import System.Exit (die)
import Data.Text ( pack )
import qualified Data.Text.IO as T
import Text.Megaparsec (errorBundlePretty)
import Parser (parseProgram)
import Printer (printProgram)
import Data (Line)

main :: IO ()
main = do
    testParseFile "test/programs/hello-world.gcode"
    testParseFile "test/programs/cycle-test.gcode"
    testParseFile "test/programs/expr-test.gcode"
    putStrLn "Done"

testParseFile :: FilePath -> IO ()
testParseFile filePath = do
    putStrLn ("Checking parser with input from: " ++ filePath)
    inputFile <- T.readFile filePath
    let parseResult = parseProgram filePath inputFile
    case parseResult of
        Left e -> putStrLn $ errorBundlePretty e
        Right r ->
            if checkParse r then do
                putStrLn "Success"
            else do
                putStrLn "A:"
                putStrLn $ show r
                putStrLn "B:"
                putStrLn $ show ((parseProgram "" . pack . printProgram) r)
                die "Parse trees don't match"
    return ()

checkParse :: [Line] -> Bool
checkParse program = Right program == (parseProgram "" . pack . printProgram) program
