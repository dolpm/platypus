{ open Parser }

let digit = ['0' - '9']
let number = digit+
let str_contents = [\x00-\x7F]*

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

  | '+'      { PLUS }
  | '-'      { MINUS }
  | '*'      { TIMES }
  | '/'      { DIVIDE }

  | '&'      { REF }
  | '@'      { DEREF }
  | '~'      { FLUID }

  | "<|"     { LPIPE }
  | "|>"     { RPIPE }

  | "=="     { EQ }
  | "!="     { NEQ }
  | '<'      { LT }
  | "<="     { LEQ }
  | '>'      { GT }
  | ">="     { GEQ }
  | "&&"     { AND }
  | "||"     { OR }

  | '!'      { NOT }

  | "as"     { AS }
  | "pipe"   { PIPE }

  | "if"     { IF }
  | "else"   { ELSE }

  | "loop"   { LOOP }
  | "while"  { WHILE }
  | "vis"    { VIS }

  | "char"   { CHAR }
  | "int"    { INT }
  | "float"  { FLOAT }
  | "bool"   { BOOL }
  | "unit"   { UNIT }
  | "tuple"  { TUPLE }
  | "string" { STRING }
  | "thing"  { THING }

  | "box"    { BOX }
  | "collection" { COLLECTION }
  | "option" { OPTION }

  | "true"   { BLIT(true)  }
  | "false"  { BLIT(false) }

  | number as lxm { LITERAL(int_of_string lxm) }
  | number '.'  digit* as lxm { FLIT(lxm) }
  | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
  | '"' str_contents '"'  as lxm { STR(lxm) }

  | eof { EOF }
  | _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  | "*/" { token lexbuf }
  | _    { comment lexbuf }