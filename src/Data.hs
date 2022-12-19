module Data (UnaryOp (..), BinaryOp (..), Expr (..), RN (..), RealExpr, Line (..), Segment (..)) where

import Data.Text ( Text )

data UnaryOp = Abs | Acos | Asin | Cos | Exp | Fix | Fup | Ln | Round | Sin | Sqrt | Tan deriving (Show, Eq)

data BinaryOp = Atan | Power | Div | Mod | Times | And | Or | Xor | Add | Subtract deriving (Show, Eq)

data Expr v = Value v
            | Param (Expr v)
            | Unary UnaryOp (Expr v)
            | Binary BinaryOp (Expr v) (Expr v)
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
