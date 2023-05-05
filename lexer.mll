
{
  open Parser;;
  exception Lexical_error;; 
}

rule token = parse
    [' ' '\t']  { token lexbuf }
  | "lambda"    { LAMBDA }
  | "L"         { LAMBDA }
  | "true"      { TRUE }
  | "false"     { FALSE }
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | "succ"      { SUCC }
  | "pred"      { PRED }
  | "iszero"    { ISZERO }
  | "let"       { LET }
  | "letrec"    { LETREC }
  | "in"        { IN }
  | "Bool"      { BOOL }
  | "Nat"       { NAT }
  | "isnil"     { ISNIL }
  | "head"      { HEAD }
  | "tail"      { TAIL }
  | "Str"       { STR }
  | "String"    { STR }
  | "List"      { LIST }
  | "list"      { LIST }
  | "unit"      { UNIT }
  | "()"        { UNIT }
  | "Unit"      { TYUNIT }
  | "++"        { CONCAT }
  | ","         { COMMA }
  | "["         { LBRACKET }
  | "]"         { RBRACKET }
  | "{"         { LKEY }
  | "}"         { RKEY }
  | '('         { LPAREN }
  | ')'         { RPAREN }
  | '.'         { DOT }
  | '='         { EQ }
  | ':'         { COLON }
  | ';'         { SEMICOLON }
  | "->"        { ARROW }
  | "\""[^'"']*"\"" { STRING (Lexing.lexeme lexbuf)}
  | ['0'-'9']+  { INTV (int_of_string (Lexing.lexeme lexbuf)) }
  | ['a'-'z']['a'-'z' '_' '0'-'9']*
                { STRINGV (Lexing.lexeme lexbuf) }
  | eof         { EOF }
  | _           { raise Lexical_error } 

