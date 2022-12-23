{-# LANGUAGE OverloadedStrings #-}

module Parser (parseProgram) where

import Control.Applicative hiding (many, some)
import Data.Text ( Text, foldl' )
import Data.Void ( Void )
import Data.Char ( isPrint, toUpper, isDigit, digitToInt )
import Data.Ratio ((%))

import Text.Megaparsec
import Text.Megaparsec.Char ( char, char', eol, hspace )
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr ( makeExprParser, Operator(InfixL) )

import Data

type Parser = Parsec Void Text

parseProgram :: String -> Text -> Either (ParseErrorBundle Text Void) [Line]
parseProgram = runParser program

sc :: Parser ()
sc = hspace

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol' sc

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

program :: Parser [Line]
program = manyTill line eof <?> "program"

line :: Parser Line
line = Line
    <$> option False (True <$ symbol "/")
    <*> optional line_number
    <*> segment `sepBy` hspace
    <* optional eol
    <?> "line"

line_number :: Parser Int
line_number = symbol "N" *> lexeme L.decimal <?> "line number"

segment :: Parser Segment
segment =  choice [comment, mid_line_word, parameter_setting] <?> "segment"

mid_line_word :: Parser Segment
mid_line_word = Word <$> (lexeme mid_line_letter) <*> real_value <?> "word"

arc_tangent_combo :: Parser RealExpr
arc_tangent_combo = Binary Atan <$> (symbol "ATAN" *> expression) <*> (symbol "/" *> expression) <?> "arctan expression"

comment :: Parser Segment
comment = (Comment <$> ordinary_comment) <|> (Message <$> message) <?> "comment"

comment_characters :: Parser Text
comment_characters = takeWhileP Nothing (\c -> isPrint c && not (c `elem` ['(', ')']))

parameter_setting :: Parser Segment
parameter_setting = ParameterSetting <$> ix <*> value <?> "parameter setting"
    where ix = symbol "#" *> parameter_index
          value = symbol "=" *> real_value

parameter_index :: Parser RealExpr
parameter_index = lexeme real_value <?> "parameter index"

message :: Parser Text
message = parens $ do
    symbol "M"
    symbol "S"
    symbol "G"
    symbol ","
    comment_characters <?> "message"

mid_line_letter :: Parser Char
mid_line_letter = satisfy ((`elem` ['A','B','C','D','F','G','H','I','J','K','L','M','P','Q','R','S','T','X','Y','Z']) . toUpper)

ordinary_comment :: Parser Text
ordinary_comment = parens comment_characters

parameter_value :: Parser RealExpr
parameter_value =  symbol "#" *> (Param <$> parameter_index) <?> "parameter value"

real_number_expr :: Parser RealExpr
real_number_expr = Value <$> lexeme real_number <?> "real number"
          
real_value :: Parser RealExpr
real_value =  choice [real_number_expr, expression, parameter_value, unary_combo] <?> "real value"

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

binary :: Text -> BinaryOp -> Operator Parser RealExpr
binary name op = InfixL (Binary op <$ symbol name)

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

unary :: Text -> UnaryOp -> Parser RealExpr
unary name op = Unary op <$> (symbol name *> expression)

negate' :: (Integer, Int) -> (Integer, Int)
negate' (d, e) = (negate d, e)

real_number :: Parser RN
real_number = do
  (d, e) <- real_number'
  return $ RN (fromInteger d * 10**(fromIntegral e))

real_number' :: Parser (Integer, Int)
real_number' = (option id sign <*>) (s1 <|> s2)
  where s1 = do
          c' <- L.decimal
          option (c', 0) (dotDecimal_ c')
        s2 = dotDecimal_ 0
        sign = (id <$ char '+') <|> (negate' <$ char '-')

dotDecimal_ :: Integer -> Parser (Integer, Int)
dotDecimal_ c' = mkNum <$> (char '.' *> takeWhile1P (Just "digit") isDigit)
  where mkNum t = foldl' step (c', 0) t
        step (a, e') c = ((a * 10 + fromIntegral (digitToInt c)), (e' - 1))