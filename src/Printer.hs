{-# LANGUAGE RecordWildCards #-}

module Printer (printProgram, printParseResult) where

import Data
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
printExpr = prt where
    fn n e = n ++ "[" ++ printExpr e ++ "]"
    op n l r = "[" ++ printExpr l ++ " " ++ n ++ " " ++ printExpr r ++ "]"
    prt (Value v)   = printRealNumber v
    prt (Param e)   = '#' : printExpr e
    prt (Abs e)     = fn "ABS" e
    prt (Acos e)    = fn "ACOS" e
    prt (Asin e)    = fn "ASIN" e
    prt (Cos e)     = fn "COS" e
    prt (Exp e)     = fn "EXP" e
    prt (Fix e)     = fn "FIX" e
    prt (Fup e)     = fn "FUP" e
    prt (Ln e)      = fn "LN" e
    prt (Round e)   = fn "ROUND" e
    prt (Sin e)     = fn "SIN" e
    prt (Sqrt e)    = fn "SQRT" e
    prt (Tan e)     = fn "TAN" e
    prt (Atan l r)  = "ATAN[" ++ printExpr l ++ "]/[" ++ printExpr r ++ "]"
    prt (Power l r) = op "**" l r
    prt (Div l r)   = op "/" l r
    prt (Mod l r)   = op "MOD" l r
    prt (Times l r) = op "*" l r
    prt (And l r)   = op "AND" l r
    prt (Or l r)    = op "OR" l r
    prt (Xor l r)   = op "XOR" l r
    prt (Add l r)   = op "+" l r
    prt (Subtract l r) = op "-" l r

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