type action = Ast | LLVM_IR [@@warning "-37"] (* todo: remove warning *)

let () =
  let action = ref LLVM_IR in
  let set_action a () = action := a in
  let speclist = [ ("-a", Arg.Unit (set_action Ast), "Print the AST") ] in
  let usage_msg = "usage: ./microc.native [-a] [file.ppus]" in
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;

  let lexbuf = Lexing.from_channel !channel in
  let ast = Parser.program Scanner.token lexbuf in
  match !action with
  | Ast -> print_string (Ast.string_of_program ast)
  | _ ->
      let _sast = Semant.check ast in
      ()
