module Data (Expr (..), RN (..), RealExpr, Line (..), Segment (..)) where

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
            deriving (Show, Eq)

data RN = RN Integer Int deriving (Show)

instance Eq RN where
    (RN 0 _) == (RN 0 _)   = True
    (RN d e) == (RN d' e') = d == d' && e == e'

type RealExpr = Expr RN

data Line = Line { blockDelete :: Bool, lineNumber :: Maybe Int, segments :: [Segment] } deriving  (Show, Eq)

data Segment = Word Char RealExpr
             | Comment Text
             | Message Text
             | ParameterSetting RealExpr RealExpr
             deriving  (Show, Eq)
