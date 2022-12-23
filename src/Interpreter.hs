module Interpreter where

import Data
import Eval ( Variables, evalExpr )

import Data.List ( foldl' )
import qualified Data.Map.Strict as Map

interpretProgram :: [Line] -> [Line]
interpretProgram p = go p Map.empty where
    go []     _ = []
    go (l:ls) vs = let (vs', l') = interpretLine vs l in l' : go ls vs'

interpretLine :: Variables -> Line -> (Variables, Line)
interpretLine vs l = (vs', l') where
    ss = map (interpretSegment vs) (segments l)
    l' = l { segments = filter (not . isSetParam) ss }
    vs' = foldl' applyParam vs ss
    isSetParam (ParameterSetting _ _) = True
    isSetParam _                      = False

interpretSegment :: Variables -> Segment -> Segment
interpretSegment vs (Word c e)             = Word c (Value $ evalExpr e vs)
interpretSegment vs (ParameterSetting p e) = ParameterSetting (Value $ evalExpr p vs) (Value $ evalExpr e vs)
interpretSegment _  s                      = s

applyParam :: Variables -> Segment -> Variables
applyParam vs (ParameterSetting (Value (RN k)) (Value v)) = Map.insert (floor k) v vs
applyParam vs _                                           = vs