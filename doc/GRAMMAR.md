line = ["/"] + [line_number] + {segment} + EOL .

line_number = "N" + digit + [digit] + [digit] + [digit] + [digit] .

segment = mid_line_word | comment | parameter_setting .

mid_line_word = mid_line_letter + real_value .

comment = message | ordinary_comment .

parameter_setting = "#" + parameter_index + "=" + real_value .

mid_line_letter = "A" | "B" | "C" | "D" | "F" | "G" | "H" | "I"
    | "J" | "K" | "L" | "M" | "P" | "Q" | "R" | "S" | "T"
    | "U" | "Y" | "Z" .

real_value = real_number | expression | parameter_value | unary_combo .

message =
    "(" + {white_space} + "M" + {white_space} + "S" +
    {white_space} + "G" + {white_space} + "," + {comment_character} +
    ")" .

ordinary_comment = "(" + {comment_character} + ")" .

parameter_index = real_value .

real_number =
    [ "+" | "-" ] +
    (( digit + { digit } + ["."] + {digit}) | ( "." + digit + {digit})) .

expression = "[" + real_value + { binary_operation + real_value } + "]" .

parameter_value = "#" + parameter_index .

unary_combo = ordinary_unary_combo | arc_tangent_combo .

comment_character = (*  any printable character plus space and tab, except for "(" or ")" *)

digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" .

binary_operation = ( "**" ) | ( "/" | "mod" | "*" ) | ( "and" | "xor" | "-" | "or" | "+" ) .

arc_tangent_combo = "atan" + expression + "/" + expression .

ordinary_unary_combo = ordinary_unary_operation + expression .

ordinary_unary_operation =
    "abs" | "acos" | "asin" | "cos" | "exp" |
    "fix" | "fup" | "ln" | "round" | "sin" | "sqrt" | "tan" .

white_space = SPC | TAB