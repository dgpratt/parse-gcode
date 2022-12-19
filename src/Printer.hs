{-# LANGUAGE RecordWildCards #-}

module Printer (printProgram, printParseResult) where

import Data
import Data.Char ( toUpper ) 
import Data.List ( intercalate, splitAt ) 
import Data.Text ( unpack )

import Data.Text ( Text )
import Data.Void ( Void )
import Text.Megaparsec (errorBundlePretty)
import Text.Megaparsec.Error (ParseErrorBundle)

printProgram :: [Line] -> String
printProgram = unlines . map printLine

printParseResult :: Either (ParseErrorBundle Text Void) [Line] -> String
printParseResult (Left e) = errorBundlePretty e
printParseResult (Right r) = printProgram r

printLine :: Line -> String
printLine Line {..} = bd ++ ln ++ segs
    where bd = (if blockDelete then "/" else "")
          ln = maybe "" (\n -> "N" ++ padLeft '0' 4 (show n) ++ " ") lineNumber
          segs = intercalate " " (map printSegment segments)

printSegment :: Segment -> String
printSegment (Word c (Value v)) = c : printRealNumber v
printSegment (Word c e) = c : ' ': printExpr e
printSegment (Comment t) = "(" ++ unpack t ++ ")"
printSegment (Message t) = "( MSG, " ++ unpack t ++ ")"
printSegment (ParameterSetting t v) = "#" ++ printExpr t ++ "=" ++ printExpr v

printExpr :: RealExpr -> String
printExpr (Value v)   = printRealNumber v
printExpr (Param e)   = '#' : printExpr e
printExpr (Binary Atan l r)  = "ATAN[" ++ printExpr l ++ "]/[" ++ printExpr r ++ "]"
printExpr (Unary op e)   = printUnaryOp op ++ "[" ++ printExpr e ++ "]"
printExpr (Binary op l r)   = "[" ++ printExpr l ++ " " ++ printBinaryOp op ++ " " ++ printExpr r ++ "]"

printUnaryOp :: UnaryOp -> String
printUnaryOp = map toUpper . show

printBinaryOp :: BinaryOp -> String
printBinaryOp Atan  = "ATAN"
printBinaryOp Power = "**"
printBinaryOp Div   = "/"
printBinaryOp Mod   = "MOD"
printBinaryOp Times = "*"
printBinaryOp And   = "AND"
printBinaryOp Or    = "OR"
printBinaryOp Xor   = "XOR"
printBinaryOp Add   = "+"
printBinaryOp Subtract = "-"

printRealNumber :: RN -> String
printRealNumber (RN 0 _)         = "0"
printRealNumber (RN d 0)         = show d
printRealNumber (RN d e) | d < 0 = "-" ++ printRealNumber (RN (-d) e)
printRealNumber (RN d e)         =
    if p == 0 then "0." ++ r else l ++ "." ++ r
    where s     = show d
          p     = (length s) + e
          (l,r) = splitAt p s

padLeft :: a -> Int -> [a] -> [a]
padLeft c n s = replicate (n - length s) c ++ s