%{ open Ast %}

%token SEMI RANGE COLON LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA PLUS MINUS TIMES DIVIDE ASSIGN REASSIGN LPIPE RPIPE
%token NOT EQ NEQ LT LEQ GT GEQ AND OR REF DEREF FLUID THING
%token IF ELSE NOELSE WHILE CHAR INT STRING BOOL FLOAT TUPLE UNIT BOX VECTOR OPTION LOOP AS PIPE
%token <int> INTLIT
%token <char> CHARLIT
%token <bool> BOOLLIT
%token <string> IDENT FLOATLIT STRINGLIT LIFETIME
%token UNITLIT
%token EOF

%start program
%type <Ast.program> program

%nonassoc RPIPE
%nonassoc ELSE

%right LPIPE
/* %right ASSIGN REASSIGN */

%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE 
%right NOT
%right REF FLUID DEREF

%%

program:
  decls EOF { $1 }

typ:
  | INT   { Int   }
  | FLOAT { Float }
  | BOOL  { Bool }
  | TUPLE { Tuple }
  | UNIT  { Unit }
  | CHAR  { Char }
  | STRING { String }
  | VECTOR LBRACKET typ RBRACKET   { Vector($3) }
  | BOX LBRACKET typ RBRACKET  { Box($3) }
  | OPTION LBRACKET typ RBRACKET  { Option($3) }
  | REF LIFETIME typ { Ref($3, $2) }
  /* infer lifetime as static if not provided? */
  | REF typ { Ref($2, "'static") }
  | FLUID typ  { Fluid($2) }
  | THING LBRACKET IDENT RBRACKET { ThingInstance($3) }


decls:
 | { ([], [])                             } 
 | decls tdecl { (($2 :: fst $1), snd $1) }
 | decls pdecl { (fst $1, ($2 :: snd $1)) }

/* optional lifetimes? */
pdecl:
  PIPE IDENT RPIPE
    LBRACKET lifetime_opt RBRACKET RPIPE
    LBRACKET formals_opt RBRACKET RPIPE
    typ
    LBRACE
    stmt_list
    RBRACE
    {
      {
        name = $2;
        lifetimes = List.rev $5;
        formals = List.rev $9;
        return_type = $12;
        body = List.rev $14;
      }
    }

lifetime_opt:
  | { [] }
  | lifetime_list   { $1 }

lifetime_list:
  | LIFETIME                   { [$1] }
  | lifetime_list COMMA LIFETIME { $3 :: $1 }

formals_opt:
  | { [] }
  | formal_list   { $1 }

/* currently just doing false for mutable but this should be handled */
formal_list:
  | IDENT COLON typ                   { [(false,$3,$1)] }
  | formal_list COMMA IDENT COLON typ { (false,$5,$3) :: $1 }

stmt_list:
  | { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
  | expr SEMI                               { Expr $1               }
  | RPIPE expr_opt SEMI                     { PipeOut $2            }
  | LBRACE stmt_list RBRACE                 { Block(List.rev $2)    }
  | IF expr stmt %prec NOELSE { If($2, $3, Block([])) }
  | IF expr stmt ELSE stmt    { If($2, $3, $5)        }
  | LOOP expr RANGE expr AS IDENT stmt       { Loop($2, $4, $6, IntLiteral(1), $7) }
  | LOOP expr RANGE expr AS
    IDENT COMMA expr stmt      { Loop($2, $4, $6, $8, $9)   }
  | WHILE expr stmt                         { While($2, $3)         }
  | typ IDENT LPIPE expr SEMI   { Assign($1, $2, $4)         }
  | IDENT LPIPE expr SEMI   { ReAssign($1, $3)         }

expr_opt:
  | { NoExpr }
  | expr { $1 }

thing_child_assn_list:
  | IDENT COLON expr                              { [($1, $3)] }
  | thing_child_assn_list COMMA IDENT COLON expr       { ($3, $5) :: $1 }

expr:
  | INTLIT          { IntLiteral($1)            }
  | FLOATLIT	           { FloatLiteral($1)           }
  | BOOLLIT             { BoolLiteral($1)            }
  | CHARLIT            { CharLiteral($1) }
  | STRINGLIT            { StringLiteral($1) }
  | UNITLIT           { UnitLiteral }
  | IDENT               { Ident($1)                 }
  | IDENT LBRACE
     thing_child_assn_list
    RBRACE              { ThingValue($1, $3) }
  /*
  | THINGLIT            { ThingLiteral($1) } 
  | TUPLELIT            { TupleLiteral($1) 
  */
  
  | expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr MINUS  expr { Binop($1, Sub,   $3)   }
  | expr TIMES  expr { Binop($1, Mult,  $3)   }
  | expr DIVIDE expr { Binop($1, Div,   $3)   }
  | expr EQ     expr { Binop($1, Equal, $3)   }
  | expr NEQ    expr { Binop($1, Neq,   $3)   }
  | expr LT     expr { Binop($1, Lt,  $3)   }
  | expr LEQ    expr { Binop($1, Leq,   $3)   }
  | expr GT     expr { Binop($1, Gt, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3)   }
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }

  | MINUS expr %prec NOT { Unop(Neg, $2)      }
  | NOT expr         { Unop(Not, $2)          }
  | DEREF expr       { Unop(Deref, $2)      }
  | REF expr         { Unop(Ref, $2)          }

  | IDENT LPIPE LBRACKET args_opt RBRACKET { PipeIn($1, $4)  }
  | LPAREN expr RPAREN { $2                   }

args_opt:
  | { [] }
  | args_list  { List.rev $1 }

args_list:
  | expr                    { [$1] }
  | args_list COMMA expr { $3 :: $1 }

/*
  example:
  thing MyThing <| {
    x: int,
    y: string
  }
*/
tdecl:
  THING IDENT LPIPE LBRACE
    thing_child_list
  RBRACE { Thing($2, List.rev $5) }

/* currently just doing false for mutable but this should be handled */
thing_child_list:
  | IDENT COLON typ                              { [($1, $3)] }
  | thing_child_list COMMA IDENT COLON typ       { ($3, $5) :: $1 }
