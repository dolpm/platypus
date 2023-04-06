type action = Ast | Sast | LLVM_IR | Compile | Exec

let () =
  let action = ref LLVM_IR in
  let set_action a () = action := a in
  let verbosity = ref false in
  let set_verbosity () = verbosity := true in
  let keep = ref false in
  let set_keep () = keep := true in
  let speclist =
    [
      ("-a", Arg.Unit (set_action Ast), "Print the AST");
      ("-s", Arg.Unit (set_action Sast), "Print the SAST");
      ("-v", Arg.Unit set_verbosity, "Print the AST");
      ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
      ("-c", Arg.Unit (set_action Compile), "Compile the platypus program");
      ("-k", Arg.Unit set_keep, "Keep intermediary files");
      ( "-e",
        Arg.Unit (set_action Exec),
        "Compile and execute the platypus program" );
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

  (* create module from externally-imported files *)
  (* to be passed into translate fn for linking *)
  let m_external =
    Llvm_irreader.parse_ir (Llvm.global_context ())
      (Llvm.MemoryBuffer.of_string Builtins.as_llvm_ir)
  in
  let _ = Llvm_analysis.assert_valid_module m_external in

  match !action with
  | Ast -> print_string (Ast.string_of_program ast)
  | c -> (
      let sast = Semant.check ast !verbosity in
      match c with
      | Sast -> print_string (Sast.string_of_sprogram sast)
      | LLVM_IR ->
          print_string
            (Llvm.string_of_llmodule (Codegen.translate sast m_external))
      | c -> (
          let m = Codegen.translate sast m_external in
          let _ = Llvm_analysis.assert_valid_module m in

          match c with
          | Compile ->
              let _bc = Llvm_bitwriter.write_bitcode_file m (!f_name ^ ".bc") in
              let _ =
                Sys.command
                  ("llc -filetype=obj " ^ !f_name ^ ".bc -o  " ^ !f_name
                 ^ ".o && clang  " ^ !f_name ^ ".o -o " ^ !f_name)
              in
              (* clean up tmp files if -k not present *)
              if not !keep then
                let _ = Sys.remove (!f_name ^ ".bc") in
                Sys.remove (!f_name ^ ".o")
          | Exec ->
              (* create the jit *)
              let jit =
                match Llvm_executionengine.initialize () with
                | true -> Llvm_executionengine.create m
                | false -> raise (Failure "failed to initialize jit")
              in

              (* run the main function *)
              let _ =
                Llvm_executionengine.get_function_address "main"
                  (Foreign.funptr Ctypes.(void @-> returning void))
                  jit ()
              in

              (* clean up *)
              Llvm_executionengine.dispose jit
          | _ -> ()))
