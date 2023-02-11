%{ open Ast %}

%token SEMI DOT LPAREN RPAREN LBRACE RBRACE COMMA PLUS MINUS TIMES DIVIDE ASSIGN LPIPE RPIPE
%token NOT EQ NEQ LT LEQ GT GEQ AND OR REF FLUID THING
%token IF ELSE WHILE CHAR INT STRING BOOL FLOAT TUPLE UNIT BOX VECTOR OPTION LOOP AS PIPE
%token <int> INTLIT
%token <char> CHARLIT
%token <bool> BOOLLIT
%token <string> ID FLOATLIT STRINGLIT
%token UNITLIT
%token EOF

%start program
%type <Ast.program> program

%nonassoc RPIPE
%nonassoc ELSE

%right LPIPE
%right ASSIGN

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
  | VECTOR  { Vector($1) }
  | FLUID  { Fluid($1) }
  | REF  { Ref($1) }
  | THING  { Thing($1) }
  | BOX  { Box($1) }
  | OPTION  { Option($1) }


decls:
 | /* nothing */ { ([], [])               }
 | decls tdecl { (($2 :: fst $1), snd $1) }
 | decls pdecl { (fst $1, ($2 :: snd $1)) }

(* START pipe declarations *)
(*
  TODO:
    - should lifetimes be optional if they aren't needed?
*)
pdecl:
  PIPE ID RPIPE
    LBRACKET lifetime_opt RBRACKET RPIPE
    LBRACKET formals_opt RBRACKET RPIPE
    typ
    LBRACE
    stmt_list
    RBRACE
    {
      {
        lifetiems = List.rev $5;
        formals = List.rev $9;
        return_type = $12;
        body = List.rev $14;
      }
    }

lifetime_opt:
  | { [] }
  | lifetime_list   { $1 }

lifetime_list:
  | string                   { [$1] }
  | lifetime_list COMMA string { $3 :: $1 }

formals_opt:
  | { [] }
  | formal_list   { [$1] }

formal_list:
  | typ ID                   { [($1,$2)] }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
  | expr SEMI                               { Expr $1               }
  | RPIPE expr_opt SEMI                     { PipeOut $2            }
  | LBRACE stmt_list RBRACE                 { Block(List.rev $2)    }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7)        }
  | LOOP expr DOT DOT expr AS
    LPAREN expr COMMA expr RPAREN stmt      { For($2, $5, $8, $10, $12)   }
  | LOOP expr DOT DOT expr AS expr stmt     { For($2, $5, $7, IntLiteral(1), $8) }
  | WHILE LPAREN expr RPAREN stmt           { While($3, $5)         }

expr_opt:
  | { Noexpr }
  | expr { $1 }

expr:
  | INTLIT          { IntLitearl($1)            }
  | FLOATLIT	           { Floatliteral($1)           }
  | BOOLLIT             { BoolLiteral($1)            }
  | CHARLIT            { CharLiteral($1) }
  | STRINGLIT            { StringLiteral($1) }
  | UNITLIT           { UnitLiteral }
  | ID               { Id($1)                 }
  
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

  | ID LPIPE expr   { Assign($1, $3)         }
  | ID LPIPE LBRACKET args_opt RBRACKET { PipeIn($1, $4)  }
  | LPAREN expr RPAREN { $2                   }

args_opt:
  | { [] }
  | args_list  { List.rev $1 }

args_list:
  | expr                    { [$1] }
  | args_list COMMA expr { $3 :: $1 }

(*
  gonna omit visibility keyword for now since
  we don't have import/export

  example:
  thing MyThing <| {
    x: int,
    y: string,
  }
*)
tdecl:
  THING ID LPIPE
  LBRACE

  RBRACE

(* todo how do we allow user-defined types in here (like other things)? *)
thing_child_decl:
  | ID COLON typ

thing_child_list:
  | thing_child_decl                        { [$1] }
  | thing_child_list COMMA thing_child_decl { $3 :: $1 }
