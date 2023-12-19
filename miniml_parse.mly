(*
                           MiniML -- Parser
*)
                  
%{
  open Expr ;;
%}

%token EOF
%token OPEN CLOSE
%token LET DOT IN REC
%token NEG
%token PLUS MINUS 
%token TIMES DIVIDES
%token FL_PLUS FL_MINUS
%token FL_TIMES FL_DIVIDES
%token FL_NEG
%token LESSTHAN GREATERTHAN EQUALS
%token IF THEN ELSE 
%token FUNCTION
%token RAISE
%token <string> ID
%token <int> INT 
%token <float> FLOAT
%token <string> STRING
%token TRUE FALSE
%token CONCAT
%token UNIT
%token REF 
%token DEREF
%token ASSIGN

%nonassoc IF
%left LESSTHAN EQUALS
%left PLUS MINUS
%left TIMES
%nonassoc NEG

%start input
%type <Expr.expr> input

(* Grammar follows *)
%%
input:  exp EOF                 { $1 }

exp:    exp expnoapp            { App($1, $2) }
        | expnoapp              { $1 }

expnoapp: INT                   { Num $1 }
        | TRUE                  { Bool true }
        | FLOAT                 { Float $1 }
        | FALSE                 { Bool false }
        | ID                    { Var $1 }
        | STRING                { String $1 }
        | UNIT                  { Unit $1 }
        | exp PLUS exp          { Binop(Plus, $1, $3) }
        | exp FL_PLUS exp       { Binop(Fl_plus, $1, $3) }
        | exp MINUS exp         { Binop(Minus, $1, $3) }
        | exp FL_MINUS exp      { Binop(Fl_minus, $1, $3) }
        | exp TIMES exp         { Binop(Times, $1, $3) }
        | exp FL_TIMES exp      { Binop(Fl_times, $1, $3) }
        | exp DIVIDES exp       { Binop(Divides, $1, $3) }
        | exp FL_DIVIDES exp    { Binop(Fl_divides, $1, $3) }
        | exp EQUALS exp        { Binop(Equals, $1, $3) }
        | exp LESSTHAN exp      { Binop(LessThan, $1, $3) }
        | exp GREATERTHAN exp   { Binop(GreaterThan, $1, $3) }
        | exp CONCAT exp        { Binop(Concat, $1, $3) }
        | NEG exp               { Unop(Negate, $2) }
        | FL_NEG exp            { Unop(Negate, $2) }
        | IF exp THEN exp ELSE exp      { Conditional($2, $4, $6) }
        | LET ID EQUALS exp IN exp      { Let($2, $4, $6) }
        | LET REC ID EQUALS exp IN exp  { Letrec($3, $5, $7) }
        | FUNCTION ID DOT exp   { Fun($2, $4) } 
        | RAISE                 { Raise }
        | OPEN exp CLOSE        { $2 }
        | LET ID EQUALS REF exp { Ref($2, $5) }
        | ID ASSIGN exp         { Ref($1, $3) }
        | DEREF ID              { Deref($2) }
;

%%
