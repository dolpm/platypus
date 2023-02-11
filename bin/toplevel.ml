type action = Ast | Sast | LLVM_IR | Compile

let () =
  let action = ref Compile in
  let set_action a () = action := a in
  let speclist = [ ("-a", Arg.Unit (set_action Ast), "Print the AST") ] in
  let usage_msg = "usage: ./microc.native [-a] [file.ppus]" in
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;

  let lexbuf = Lexing.from_channel !channel in
  let ast = Parser.program Scanner.token lexbuf in
  match !action with
  | Ast -> print_string (Ast.string_of_program ast)
  | _ -> (
      let sast = Semant.check ast in
      match !action with
      | Ast -> ()
      | Sast -> print_string (Sast.string_of_sprogram sast)
      | LLVM_IR ->
          print_string (Llvm.string_of_llmodule (Codegen.translate sast))
      | Compile ->
          let m = Codegen.translate sast in
          Llvm_analysis.assert_valid_module m;
          print_string (Llvm.string_of_llmodule m))
