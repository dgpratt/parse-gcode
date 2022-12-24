module Interpreter where

import Data
import Eval ( Variables, evalExpr )

import Data.List ( foldl' )
import qualified Data.Map.Strict as Map

-- | Evaluate all expressions in the program and remove parameter assignments.
interpretProgram :: [Line] -> [Line]
interpretProgram p = go p Map.empty where
    go []     _ = []
    go (l:ls) vs = let (vs', l') = interpretLine vs l in l' : go ls vs'

-- | Evaluate all expressions in the line and remove parameter assignments.
interpretLine :: Variables -> Line -> (Variables, Line)
interpretLine vs l = (vs', l') where
    ss = map (interpretSegment vs) (segments l) -- interpret each segment in the source line
    l' = l { segments = filter (not . isSetParam) ss } -- remove "set param" commands from the new line
    vs' = foldl' applyParam vs ss -- apply "set param" commands to the variable set
    isSetParam (ParameterSetting _ _) = True
    isSetParam _                      = False

-- | Evaluate all expressions in the segment.
interpretSegment :: Variables -> Segment -> Segment
interpretSegment vs (Word c e)             = Word c (Value $ evalExpr e vs)
interpretSegment vs (ParameterSetting p e) = ParameterSetting (Value $ evalExpr p vs) (Value $ evalExpr e vs)
interpretSegment _  s                      = s

-- | Apply parameter assignments to the variable set.
applyParam :: Variables -> Segment -> Variables
applyParam vs (ParameterSetting (Value (RN k)) (Value v)) = Map.insert (floor k) v vs
applyParam vs _                                           = vs