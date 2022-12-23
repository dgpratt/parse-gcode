module Eval where

import Data
import qualified Data.Map.Strict as Map
import Data.Maybe ( fromMaybe )
import Data.Number.CReal ( CReal )
import Data.Fixed ( mod' )
import Data.Bits ( (.^.), (.|.), xor )

evalExpr :: RealExpr -> Map.Map Int RN -> RN
evalExpr (Value v) m       = v
evalExpr (Param e) m       = let v = evalExpr e m in Map.lookup (toInt v) m `orElse` RN 0
evalExpr (Unary op e) m    = let v = evalExpr e m in evalUnaryExpr op v
evalExpr (Binary op l r) m = let (lv, rv) = (evalExpr l m, evalExpr r m) in evalBinaryExpr op lv rv

evalUnaryExpr :: UnaryOp -> RN -> RN
evalUnaryExpr = eval where
    eval Abs   = rnEval abs
    eval Acos  = rnEval acos
    eval Asin  = rnEval asin
    eval Cos   = rnEval cos
    eval Exp   = rnEval exp
    eval Fix   = rnEval ( fromIntegral . floor )
    eval Fup   = rnEval ( fromIntegral . ceiling )
    eval Ln    = rnEval log
    eval Round = rnEval ( fromIntegral . round )
    eval Sin   = rnEval sin
    eval Sqrt  = rnEval sqrt
    eval Tan   = rnEval tan
    rnEval f = RN . f . unRn

evalBinaryExpr :: BinaryOp -> RN -> RN -> RN
evalBinaryExpr op (RN l) (RN r) = eval op where
    eval Atan     = rnEval atan2
    eval Power    = rnEval (**)
    eval Div      = rnEval (/)
    eval Mod      = rnEval mod'
    eval Times    = rnEval (*)
    eval And      = rnEval' (.^.)
    eval Or       = rnEval' (.|.)
    eval Xor      = rnEval' xor
    eval Add      = rnEval (+)
    eval Subtract = rnEval (-)
    rnEval f = RN (f l r)
    rnEval' :: (Int -> Int -> Int) -> RN
    rnEval' f = RN . fromIntegral $ (f (floor l) (floor r))

orElse :: Maybe a -> a -> a
orElse = flip fromMaybe

toInt :: RN -> Int
toInt = floor . unRn