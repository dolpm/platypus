{ open Parser }

let digit = ['0' - '9']
let number = digit+
(* todo: make sure all ascii's can be accepted here (0-255) MINUS \x22 *)
let character = [^'\x22']
let str = character*
let ident = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse
  | [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
  | "/*"     { comment lexbuf }           (* Comments *)

  | '('      { LPAREN }
  | ')'      { RPAREN }
  | '{'      { LBRACE }
  | '}'      { RBRACE }
  | '['      { LBRACKET }
  | ']'      { RBRACKET }
  | ';'      { SEMI }
  | ','      { COMMA }
  | ':'      { COLON }
  | '.'      { DOT }
  | "->"     { RANGE }

  | '+'      { PLUS }
  | '-'      { MINUS }
  | '*'      { TIMES }
  | '/'      { DIVIDE }

  | '^'     { CONCAT }

  | '&'      { BORROW }
  | '@'      { DEREF }
  | "~&"     { MUTBORROW }
  | "%"      { CLONE } 

  | "mut"    { MUT }

  | '|'      { PIPE }
  | "<|"     { LPIPE }
  | "|>"     { RPIPE }

  | "=="     { EQ }
  | "!="     { NEQ }
  | '<'      { LT }
  | "<="     { LEQ }
  | '>'      { GT }
  | ">="     { GEQ }
  | "and"     { AND }
  | "or"     { OR }
  | "||"     { CONCAT } 
  | "="      { EQUAL }

  | '!'      { NOT }

  | "as"     { AS }
  | "pipe"   { PIPE }

  | "if"     { IF }
  | "else"   { ELSE }

  | "loop"   { LOOP }
  | "while"  { WHILE }

  | "char"   { CHAR }
  | "int"    { INT }
  | "float"  { FLOAT }
  | "bool"   { BOOL }
  | "unit"   { UNIT }
  | "tuple"  { TUPLE }
  | "string" { STRING }
  | "str"    { STR }
  | "thing"  { THING }

  | "box"    { BOX }
  | "vector" { VECTOR }

  | "()"     { UNITLIT }
  | "true"   { BOOLLIT(true)  }
  | "false"  { BOOLLIT(false) }
  | '\x27' character '\x27' as lxm { CHARLIT(String.get lxm 1) }
  | '\x27' ("\\n"|"\\r") '\x27' as lxm { CHARLIT(match (String.sub lxm 1 ((String.length lxm) - 2)) with
                                                  | "\\n" -> '\n' 
                                                  | "\\r" -> '\r' 
                                                  | _ -> raise (Failure("panic! impossible")))
                                       }
  | number as lxm { INTLIT(int_of_string lxm) }
  | number '.'  digit* as lxm { FLOATLIT(lxm) }
  | '\x27' ident  as lxm { LIFETIME(lxm) }
  (* todo tuple and thing literal regex *)
  | ident as lxm { IDENT(lxm) }
  | '\x22' str '\x22' as lxm { STRINGLIT(String.sub lxm 1 ((String.length lxm) - 2)) }
  | eof { EOF }
  | _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  | "*/" { token lexbuf }
  | _    { comment lexbuf }