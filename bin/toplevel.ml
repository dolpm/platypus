type action = Ast

let () =
  let action = ref Ast in
  let set_action a () = action := a in
  let speclist = [ ("-a", Arg.Unit (set_action Ast), "Print the AST") ] in
  let usage_msg = "usage: ./microc.native [-a] [file.ppus]" in
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;

  let lexbuf = Lexing.from_channel !channel in
  let ast = Parser.program Scanner.token lexbuf in
  match !action with Ast -> print_string (Ast.string_of_program ast)
