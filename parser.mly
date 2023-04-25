
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
%token ISNIL

%token COMMA
%token LKEY
%token RKEY
%token LPAREN
%token RPAREN
%token DOT
%token EQ
%token COLON
%token ARROW
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
    atomicTerm
      { $1 }
  | SUCC atomicTerm
      { TmSucc $2 }
  | PRED atomicTerm
      { TmPred $2 }
  | ISZERO atomicTerm
      { TmIsZero $2 }
  | appTerm atomicTerm
      { TmApp ($1, $2) }

/*para las proyecciones, cambiar los atomic por path arriba*/
/*pathTerm :
    atomicTerm*/



atomicTerm :
    LPAREN term RPAREN
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

tuples:
    atomicTerm COMMA tuples
        { TmTuple ($1, $3) }
    | atomicTerm
        { TmTuple ($1, TmNil) }
    | 
        { TmTuple (TmNil, TmNil) }

ty :
    atomicTy
      { $1 }
  | atomicTy ARROW ty
      { TyArr ($1, $3) }
  | LKEY tytuples RKEY
      { $2 }

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

