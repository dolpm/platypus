type action = Ast | Sast | LLVM_IR | Compile

let () =
  let action = ref LLVM_IR in
  let set_action a () = action := a in
  let verbosity = ref false in
  let set_verbosity () = verbosity := true in
  let speclist =
    [
      ("-a", Arg.Unit (set_action Ast), "Print the AST");
      ("-s", Arg.Unit (set_action Sast), "Print the SAST");
      ("-v", Arg.Unit set_verbosity, "Print the AST");
      ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
      ("-c", Arg.Unit (set_action Compile), "Compile the platypus program");
    ]
  in
  let usage_msg = "usage: ./platypus.native [-a] [file.ppus]" in
  let channel = ref stdin in
  let f_name = ref "" in
  Arg.parse speclist
    (fun filename ->
      let _ =
        f_name := Filename.remove_extension (Filename.basename filename)
      in
      channel := open_in filename)
    usage_msg;
  let lexbuf = Lexing.from_channel !channel in
  let ast = Parser.program Scanner.token lexbuf in
  match !action with
  | Ast -> print_string (Ast.string_of_program ast)
  | c -> (
      let sast = Semant.check ast !verbosity in
      match c with
      | Sast -> print_string (Sast.string_of_sprogram sast)
      | LLVM_IR ->
          print_string (Llvm.string_of_llmodule (Codegen.translate sast))
      | Compile ->
          let m = Codegen.translate sast in
          Llvm_analysis.assert_valid_module m;
          let _bc = Llvm_bitwriter.write_bitcode_file m "tmp.bc" in
          let _ =
            Sys.command
              ("llc -filetype=obj tmp.bc -o tmp.o && clang tmp.o -o "
             ^ !f_name)
          in
          (* clean up tmp files *)
          let _ = Sys.remove "tmp.bc" in
          Sys.remove "tmp.o"
      | _ -> ())
