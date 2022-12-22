module Eval where

import Data
import qualified Data.Map.Strict as Map
import Data.Maybe ( fromMaybe )
import Data.Number.CReal ( CReal )
import Data.Fixed ( mod' )
import Data.Bits ( (.^.), (.|.), xor )

eval :: RealExpr -> Map.Map Int RN -> RN
eval (Value v) m       = v
eval (Param e) m       = let (RN c) = (eval e m) in fromMaybe (RN 0) (Map.lookup (floor c) m)
eval (Unary op e) m    = let v = (eval e m) in evalUnary op v
eval (Binary op l r) m = let (lv, rv) = (eval l m, eval r m) in evalBinary op lv rv

evalUnary :: UnaryOp -> RN -> RN
evalUnary op = go op where
    go Abs   = rn abs
    go Acos  = rn acos
    go Asin  = rn asin
    go Cos   = rn cos
    go Exp   = rn exp
    go Fix   = rn ( fromIntegral . floor )
    go Fup   = rn ( fromIntegral . ceiling )
    go Ln    = rn log
    go Round = rn ( fromIntegral . round )
    go Sin   = rn sin
    go Sqrt  = rn sqrt
    go Tan   = rn tan
    rn f = RN . f . unRn

evalBinary :: BinaryOp -> RN -> RN -> RN
evalBinary op (RN l) (RN r) = go op where
    go Atan     = rn atan2
    go Power    = rn (**)
    go Div      = rn (/)
    go Mod      = rn mod'
    go Times    = rn (*)
    go And      = rn' (.^.)
    go Or       = rn' (.|.)
    go Xor      = rn' xor
    go Add      = rn (+)
    go Subtract = rn (-)
    rn f = RN (f l r)
    rn' :: (Int -> Int -> Int) -> RN
    rn' f = RN . fromIntegral $ (f (floor l) (floor r))
