module Data (Expr (..), RealNumber, RealExpr, Line (..), Segment (..)) where

import Data.Text ( Text )

data Expr v = Value v
            | Param (Expr v)
            | Abs (Expr v)
            | Acos (Expr v)
            | Asin (Expr v)
            | Cos (Expr v)
            | Exp (Expr v)
            | Fix (Expr v)
            | Fup (Expr v)
            | Ln (Expr v)
            | Round (Expr v)
            | Sin (Expr v)
            | Sqrt (Expr v)
            | Tan (Expr v)
            | Atan (Expr v) (Expr v)
            | Power (Expr v) (Expr v)
            | Div (Expr v) (Expr v)
            | Mod (Expr v) (Expr v)
            | Times (Expr v) (Expr v)
            | And (Expr v) (Expr v)
            | Or (Expr v) (Expr v)
            | Xor (Expr v) (Expr v)
            | Add (Expr v) (Expr v)
            | Subtract (Expr v) (Expr v)
            deriving Show

type RealNumber = (Int, Int)

type RealExpr = Expr RealNumber

data Line = Line { blockDelete :: Bool, lineNumber :: Maybe Int, segments :: [Segment] } deriving Show

data Segment = Word Char RealExpr
             | Comment Text
             | Message Text
             | ParameterSetting RealExpr RealExpr
             deriving Show
