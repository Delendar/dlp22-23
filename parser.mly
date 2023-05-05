
%{
  open Lambda;;
%}

%token LAMBDA
%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE
%token SUCC
%token PRED
%token ISZERO
%token LET
%token LETREC
%token IN
%token BOOL
%token NAT
%token STR
%token LIST
%token ISNIL
%token HEAD
%token TAIL
%token UNIT
%token TYUNIT

%token COMMA
%token LBRACKET
%token RBRACKET
%token LKEY
%token RKEY
%token LBRACKET
%token RBRACKET
%token LPAREN
%token RPAREN
%token DOT
%token EQ
%token COLON
%token SEMICOLON
%token ARROW
%token CONCAT
%token EOF

%token <int> INTV
%token <string> STRINGV
%token <string> STRING

%start s
%type <Lambda.command> s

%%

s :
    STRINGV EQ term EOF
      { Bind ($1, $3) }
  | term EOF
      { Eval $1 }

term :
    appTerm
      { $1 }
  | IF term THEN term ELSE term
      { TmIf ($2, $4, $6) }
  | LAMBDA STRINGV COLON ty DOT term
      { TmAbs ($2, $4, $6) }
  | LET STRINGV EQ term IN term
      { TmLetIn ($2, $4, $6) }
  | LETREC STRINGV COLON ty EQ term IN term
      { TmLetIn ($2, TmFix (TmAbs ($2, $4, $6)), $8) }

appTerm :
    pathTerm
      { $1 }
  | SUCC pathTerm
      { TmSucc $2 }
  | PRED pathTerm
      { TmPred $2 }
  | ISZERO pathTerm
      { TmIsZero $2 }
  | LPAREN CONCAT RPAREN pathTerm pathTerm
      { TmConcat ($4, $5) }
  | pathTerm CONCAT pathTerm
      { TmConcat ($1, $3) }
  | HEAD atomicTerm
      {TmHead $2}
  | TAIL atomicTerm
      {TmTail $2}
  | ISNIL atomicTerm
      {TmIsNil $2}
  | LPAREN COLON COLON RPAREN pathTerm pathTerm
      { TmCons ($5, $6) }
  | pathTerm COLON COLON pathTerm
      { TmCons ($1, $4) }
  | appTerm pathTerm
      { TmApp ($1, $2) }

pathTerm :
    atomicTerm
        { $1 }
  | atomicTerm DOT atomicTerm
        { TmProj ($1, $3) }
  | atomicTerm SEMICOLON atomicTerm
        { TmApp (TmAbs ("x", TyUnit, $3), $1) }

atomicTerm :
    LPAREN term RPAREN
      { $2 }
  | LBRACKET records RBRACKET
      { $2 }
  | LKEY tuples RKEY
      { $2 }
  | TRUE
      { TmTrue }
  | FALSE
      { TmFalse }
  | STRINGV
      { TmVar $1 }
  | STRING
      { TmString (List.nth(String.split_on_char '\"' $1) 1) }
  | INTV
      { let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f $1 }
  | LBRACKET list RBRACKET
      { $2 }
  | UNIT
      { TmUnit }

records:
    STRINGV EQ atomicTerm COMMA records
      { TmRecord ( ($1, $3) , $5) }
  | STRINGV EQ atomicTerm
      { TmRecord ( ($1, $3) , TmNil)}

tuples:
    atomicTerm COMMA tuples
        { TmTuple ($1, $3) }
    | atomicTerm
        { TmTuple ($1, TmNil) }
    | 
        { TmTuple (TmNil, TmNil) }

list:
    | atomicTerm SEMICOLON list
        { TmCons ($1, $3) }
    | atomicTerm
        { TmCons ($1, TmNil) }
    | 
        { TmCons (TmNil, TmNil) }

ty :
    atomicTy
      { $1 }
  | atomicTy ARROW ty
      { TyArr ($1, $3) }
  | LKEY tytuples RKEY
      { $2 }
  | LIST LBRACKET ty RBRACKET
      { TyList ($3)}

tytuples:
    atomicTy COMMA tytuples
        { TyTuple ($1, $3) }
    | atomicTy
        { TyTuple ($1, TyNil) }
    | 
        { TyNil }

atomicTy :
    LPAREN ty RPAREN  
      { $2 } 
  | BOOL
      { TyBool }
  | NAT
      { TyNat }
  | STR
      { TyString }
  | LKEY tyTuples RKEY  
      { $2 }
  | LKEY tyRecord RKEY  
      { $2 }
  | TYUNIT
      { TyUnit }


tyTuples:
    atomicTy COMMA tyTuples
        { TyTuple ($1, $3) }
    | atomicTy
        { TyTuple ($1, TyNil) }
    | 
        { TyNil }

tyRecord:
    STRINGV COLON atomicTy COMMA tyRecord
      { TyRecord (($1,$3), $5) }
  | STRINGV COLON atomicTy
      { TyRecord (($1,$3), TyNil) }