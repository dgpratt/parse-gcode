{-# LANGUAGE OverloadedStrings #-}

module Parser (parseProgram) where

import Control.Applicative hiding (many, some)
import Data.Text ( Text )
import Data.Void ( Void )
import Data.Char ( isPrint )

import Text.Megaparsec
import Text.Megaparsec.Char ( char, char', eol, hspace, space, string, string' )
import Text.Megaparsec.Char.Lexer (signed, decimal)
import Control.Monad.Combinators.Expr ( makeExprParser, Operator(InfixL) )

import Data

type Parser = Parsec Void Text

parseProgram :: String -> Text -> Either (ParseErrorBundle Text Void) [Line]
parseProgram n t = runParser program n t

brackets :: Parser a -> Parser a
brackets p = char '[' *> p <* char ']'

parens :: Parser a -> Parser a
parens p = char '(' *> p <* char ')'

program :: Parser [Line]
program = manyTill line eof <?> "program"

line :: Parser Line
line = Line
    <$> option False (True <$ char '/') <* hspace
    <*> optional line_number <* hspace
    <*> segment `sepBy` hspace
    <* optional eol
    <?> "line"

line_number :: Parser Int
line_number = char' 'N' *> decimal <?> "line number"

segment :: Parser Segment
segment =  choice [comment, mid_line_word, parameter_setting] <?> "segment"

mid_line_word :: Parser Segment
mid_line_word = Word <$> (mid_line_letter <* space) <*> real_value <?> "word"

arc_tangent_combo :: Parser RealExpr
arc_tangent_combo = Atan <$> (string' "ATAN" *> expression) <*> (string "/" *> expression) <?> "arctan expression"

comment :: Parser Segment
comment = (Comment <$> ordinary_comment) <|> (Message <$> message) <?> "comment"

comment_characters :: Parser Text
comment_characters = takeWhileP Nothing (\c -> isPrint c && not (c `elem` ['(', ')']))

parameter_setting :: Parser Segment
parameter_setting = ParameterSetting <$> ix <*> value <?> "parameter setting"
    where ix = char '#' *> parameter_index
          value = char '=' *> real_value

parameter_index :: Parser RealExpr
parameter_index = real_value <?> "parameter index"

message :: Parser Text
message = parens $ do
    space
    char' 'M'
    space
    char' 'S'
    space
    char' 'G'
    space
    char ','
    comment_characters <?> "message"

mid_line_letter :: Parser Char
mid_line_letter = choice (map char' ['A','B','C','D','F','G','H','I','J','K','L','M','P','Q','R','S','T','X','Y','Z'])

ordinary_comment :: Parser Text
ordinary_comment = parens comment_characters

parameter_value :: Parser RealExpr
parameter_value =  char '#' *> (Param <$> expression) <?> "parameter value"

real_number :: Parser RealExpr
real_number = Value <$> n <?> "real number"
    where sign = (id <$ char '+') <|> (negate <$ char '-')
          n = (,) <$> (option id sign <*> decimal) <*> f
          f = option 0 (char '.' *> decimal)
          
real_value :: Parser RealExpr
real_value =  choice [real_number, expression, parameter_value, unary_combo] <* space <?> "real value"

unary_combo :: Parser RealExpr
unary_combo = choice [ordinary_unary_combo, arc_tangent_combo] <?> "unary expression"

expression :: Parser RealExpr
expression = brackets (makeExprParser real_value binaryOpTable) <?> "expression"

binaryOpTable :: [[Operator Parser RealExpr]]
binaryOpTable = [ [ binary  "**"   Power ]
                , [ binary  "/"    Div
                  , binary  "MOD"  Mod
                  , binary  "*"    Times  ]
                , [ binary  "AND"  And
                  , binary  "XOR"  Xor
                  , binary  "OR"   Or
                  , binary  "+"    Add
                  , binary  "-"    Subtract ] ]

binary :: Text -> (RealExpr -> RealExpr -> RealExpr) -> Operator Parser RealExpr
binary name f = InfixL (f <$ string' name <* space)

ordinary_unary_combo :: Parser RealExpr
ordinary_unary_combo = choice [ unary "ABS" Abs
                              , unary "ACOS" Acos
                              , unary "ASIN" Asin
                              , unary "COS" Cos
                              , unary "EXP" Exp
                              , unary "FIX" Fix
                              , unary "FUP" Fup
                              , unary "LN" Ln
                              , unary "ROUND" Round
                              , unary "SIN" Sin
                              , unary "SQRT" Sqrt
                              , unary "TAN" Tan ]

unary :: Text -> (RealExpr -> RealExpr) -> Parser RealExpr
unary name f = f <$> (string' name *> expression)