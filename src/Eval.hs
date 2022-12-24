module Eval ( Variables, evalExpr ) where

import Data
import qualified Data.Map.Strict as Map
import Data.Maybe ( fromMaybe )
import Data.Number.CReal ( CReal )

type Variables = Map.Map Int RN

evalExpr :: RealExpr -> Variables -> RN
evalExpr (Value v) _        = v
evalExpr (Param e) vs       = let v = evalExpr e vs in Map.lookup (toInt v) vs `orElse` RN 0
evalExpr (Unary op e) vs    = let v = evalExpr e vs in evalUnaryExpr op v
evalExpr (Binary op l r) vs = let (lv, rv) = (evalExpr l vs, evalExpr r vs) in evalBinaryExpr op lv rv

evalUnaryExpr :: UnaryOp -> RN -> RN
evalUnaryExpr = eval where
    eval Abs   = rnEval abs
    eval Acos  = rnEval acos_d
    eval Asin  = rnEval asin_d
    eval Cos   = rnEval cos_d
    eval Exp   = rnEval exp
    eval Fix   = rnEval ( fromIntegral . floor )
    eval Fup   = rnEval ( fromIntegral . ceiling )
    eval Ln    = rnEval log
    eval Round = rnEval ( fromIntegral . round )
    eval Sin   = rnEval sin_d
    eval Sqrt  = rnEval sqrt
    eval Tan   = rnEval tan_d
    rnEval f = RN . f . unRn

evalBinaryExpr :: BinaryOp -> RN -> RN -> RN
evalBinaryExpr op (RN l) (RN r) = eval op where
    eval Atan     = rnEval atan2_d
    eval Power    = rnEval (**)
    eval Div      = rnEval (/)
    eval Mod      = rnEval mod'
    eval Times    = rnEval (*)
    eval And      = rnEval' (&&)
    eval Or       = rnEval' (||)
    eval Xor      = rnEval' (/=)
    eval Add      = rnEval (+)
    eval Subtract = rnEval (-)
    rnEval f = RN (f l r)
    rnEval' :: (Bool -> Bool -> Bool) -> RN
    rnEval' f = RN . btr $ (f (rtb l) (rtb r))

div' :: CReal -> CReal -> Int
div' n d = floor (n / d)

mod' :: CReal -> CReal -> CReal
mod' n d = n - (fromIntegral f) * d where
    f = div' n d

orElse :: Maybe a -> a -> a
orElse = flip fromMaybe

toInt :: RN -> Int
toInt = floor . unRn

dtr :: CReal -> CReal
dtr d = (d * pi) / 180

rtd :: CReal -> CReal
rtd r = (r * 180) / pi

sin_d :: CReal -> CReal
sin_d = sin . dtr

cos_d :: CReal -> CReal
cos_d = cos . dtr

tan_d :: CReal -> CReal
tan_d = tan . dtr

asin_d :: CReal -> CReal
asin_d = rtd . asin

acos_d :: CReal -> CReal
acos_d = rtd . acos

atan2_d :: CReal -> CReal -> CReal
atan2_d = (rtd .) . atan2

rtb :: CReal -> Bool
rtb 0 = False
rtb _ = True

btr :: Bool -> CReal
btr False = 0
btr True  = 1