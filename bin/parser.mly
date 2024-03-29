/* Dylan M. | Ronit S. | Tony H. | Abe P. | Rodrigo C. */
%{ open Ast %}

%token SEMI RANGE COLON DOT LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA PLUS MINUS TIMES DIVIDE CONCAT ASSIGN REASSIGN LPIPE RPIPE MUT EQUAL
%token NOT EQ NEQ LT LEQ GT GEQ AND OR DEREF BORROW MUTBORROW THING CLONE 
%token IF ELSE NOELSE WHILE CHAR INT STRING STR BOOL FLOAT TUPLE UNIT BOX VECTOR LOOP AS PIPE
%token <int> INTLIT
%token <char> CHARLIT
%token <bool> BOOLLIT
%token <string> IDENT FLOATLIT STRINGLIT LIFETIME
%token UNITLIT
%token EOF

%start program
%type <Ast.program> program

%nonassoc NOELSE
%nonassoc ELSE
%nonassoc IDENT
%right LPIPE
%left LBRACKET RBRACKET
%left OR
%left AND
%left EQ NEQ CONCAT
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right BORROW MUTBORROW DEREF CLONE
%right NOT

%%

program:
  decls EOF { $1 }

typ:
  | INT   { Int   }
  | FLOAT { Float }
  | BOOL  { Bool }
  /* using parens instead of brackets causes reduce/reduce conflict... */
  | LBRACKET typ_list RBRACKET { Tuple(List.rev $2) }
  | UNIT  { Unit }
  | CHAR  { Char }
  | STRING { String }
  | STR { Str }
  | VECTOR LBRACKET typ RBRACKET   { Vector($3) }
  | BOX LBRACKET typ RBRACKET  { Box($3) }
  | BORROW LIFETIME typ { Borrow($3, $2) }
  | MUTBORROW LIFETIME typ { MutBorrow($3, $2) }
  /* infer lifetime as static if not provided? */
  | BORROW typ { Borrow($2, "'_") }
  | MUTBORROW typ  { MutBorrow($2, "'_")}
  | IDENT { Ident($1) }

typ_list:
  | typ                   { [$1] }
  | typ_list TIMES typ { $3 :: $1 }

decls:
 | { ([], [])                             } 
 | decls tdecl { (($2 :: fst $1), snd $1) }
 | decls pdecl { (fst $1, ($2 :: snd $1)) }

pdecl:
  PIPE IDENT
    lifetime_opt
    LBRACKET formals_opt RBRACKET RPIPE
    typ
    LBRACE
    stmt_list
    RBRACE
    {
      {
        name = $2;
        lifetimes = List.rev $3;
        formals = List.rev $5;
        return_type = $8;
        body = List.rev $10;
      }
    }

lifetime_opt:
  | { [] }
  | PIPE lifetime_list PIPE   { $2 }

lifetime_list:
  | LIFETIME                   { [$1] }
  | lifetime_list COMMA LIFETIME { $3 :: $1 }

formals_opt:
  | { [] }
  | formal_list   { $1 }

/* currently just doing false for mutable but this should be handled */
formal_list:
  | formal                   { [$1] }
  | formal_list COMMA formal { $3 :: $1 }

formal:
  | MUT IDENT COLON typ     { (true, $4, $2) }
  | IDENT COLON typ         { (false, $3, $1) }

stmt_list:
  | { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
  | expr SEMI                               { Expr $1               }
  | RPIPE expr SEMI                     { PipeOut $2            }
  | LBRACE stmt_list RBRACE                 { Block(List.rev $2)    }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7)        }
  | LOOP expr RANGE expr AS IDENT stmt       { Loop($2, $4, $6, IntLiteral(1), $7) }
  | LOOP expr RANGE expr AS
    LPAREN IDENT COMMA expr RPAREN stmt      { Loop($2, $4, $7, $9, $11)   }
  | WHILE LPAREN expr RPAREN stmt                    { While($3, $5)         }

  | typ IDENT EQUAL expr SEMI   { Assign(false, $1, $2, $4) }
  | MUT typ IDENT EQUAL expr SEMI   { Assign(true, $2, $3, $5) }
  
  /* when assigning to mutborrow, deref is required */
  | DEREF IDENT EQUAL expr SEMI   { ReAssign(true, $2, $4)         }
  | IDENT EQUAL expr SEMI   { ReAssign(false, $1, $3)         }

thing_child_assn_list:
  | IDENT COLON expr                              { [($1, $3)] }
  | thing_child_assn_list COMMA IDENT COLON expr       { ($3, $5) :: $1 }

tpl_child_list:
  | expr                          { [$1] }  
  | tpl_child_list COMMA expr     { $3 :: $1 }

expr:
  | INTLIT          { IntLiteral($1)            }
  | FLOATLIT	           { FloatLiteral($1)           }
  | BOOLLIT             { BoolLiteral($1)            }
  | CHARLIT            { CharLiteral($1) }
  | STRINGLIT            { StringLiteral($1) }
  | UNITLIT           { UnitLiteral }
  | IDENT               { Ident($1)                 }
  
  | IDENT LBRACE thing_child_assn_list RBRACE { ThingValue($1, List.rev $3) }
  | TUPLE LPAREN tpl_child_list RPAREN { TupleValue(List.rev $3) }  

  | expr LBRACKET IDENT RBRACKET { ThingAccess($1, $3) }  

  // tuple indexing
  | expr LBRACKET INTLIT RBRACKET { TupleIndex($1, $3) } 

  | expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr MINUS  expr { Binop($1, Sub,   $3)   }
  | expr TIMES  expr { Binop($1, Mult,  $3)   }
  | expr DIVIDE expr { Binop($1, Div,   $3)   }
  | expr EQ     expr { Binop($1, Equal, $3)   }
  | expr NEQ    expr { Binop($1, Neq,   $3)   }
  | expr LT     expr { Binop($1, Lt,  $3)     }
  | expr LEQ    expr { Binop($1, Leq,   $3)   }
  | expr GT     expr { Binop($1, Gt, $3)      }
  | expr GEQ    expr { Binop($1, Geq,   $3)   }
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }
  | expr CONCAT expr { Binop($1, Concat, $3)  }

  | MINUS expr %prec NOT  { Unop(Neg, $2)      }
  | NOT expr              { Unop(Not, $2)      }
  | DEREF expr            { Unop(Deref, $2)    }
  | BORROW expr           { Unop(Ref, $2)      }
  | MUTBORROW expr        { Unop(MutRef, $2)   }
  | CLONE expr            { Unop(Clone, $2)    }

  | IDENT LPIPE LBRACKET args_opt RBRACKET { PipeIn($1, $4)  }
  | LPAREN expr RPAREN { $2                   }

args_opt:
  | { [] }
  | args_list  { List.rev $1 }

args_list:
  | expr                    { [$1] }
  | args_list COMMA expr { $3 :: $1 }

tdecl:
  THING IDENT LPIPE LBRACE
    thing_child_list
  RBRACE { {tname = $2 ; elements = List.rev $5} }

/* currently just doing false for mutable but this should be handled */
thing_child_list:
  | IDENT COLON typ                              { [(false, $3, $1)] }
  | thing_child_list COMMA IDENT COLON typ       { (false, $5, $3) :: $1 }
