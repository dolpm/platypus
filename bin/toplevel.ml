(* Dylan M. | Ronit S. | Tony H. *)
type action = Ast | Sast | LLVM_IR | Compile | Exec

let () =
  let action = ref LLVM_IR in
  let set_action a () = action := a in
  let verbosity = ref false in
  let set_verbosity () = verbosity := true in
  let keep = ref false in
  let set_keep () = keep := true in
  let opt = ref false in
  let set_opt () = opt := true in
  let speclist =
    [
      ("-a", Arg.Unit (set_action Ast), "Print the AST");
      ("-s", Arg.Unit (set_action Sast), "Print the SAST");
      ("-v", Arg.Unit set_verbosity, "Print the AST");
      ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
      ("-c", Arg.Unit (set_action Compile), "Compile the platypus program");
      ("-k", Arg.Unit set_keep, "Keep intermediary files");
      ("-o", Arg.Unit set_opt, "Enable LLVM IR optimizations");
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
      let sast, ownership_map = Semant.check ast !verbosity in
      match c with
      | Sast -> print_string (Sast.string_of_sprogram sast)
      | LLVM_IR ->
          print_string
            (Llvm.string_of_llmodule
               (Codegen.translate sast ownership_map m_external))
      | c -> (
          let m = Codegen.translate sast ownership_map m_external in
          let _ = Llvm_analysis.assert_valid_module m in
          let _ = Llvm_linker.link_modules' m m_external in

          let _ =
            if !opt then
              (* create passmgrs *)
              let module_passmgr = Llvm.PassManager.create () in
              let func_passmgr = Llvm.PassManager.create_function m in
              let lto_passmgr = Llvm.PassManager.create () in
              let pm_builder = Llvm_passmgr_builder.create () in

              (* set up optimization config *)
              let _ = Llvm_passmgr_builder.set_opt_level 3 pm_builder in
              let _ = Llvm_passmgr_builder.set_size_level 1 pm_builder in
              let _ =
                Llvm_passmgr_builder.set_disable_unit_at_a_time false pm_builder
              in
              let _ =
                Llvm_passmgr_builder.set_disable_unroll_loops false pm_builder
              in
              let _ =
                Llvm_passmgr_builder.use_inliner_with_threshold 10 pm_builder
              in
              let _ =
                Llvm_passmgr_builder.populate_function_pass_manager func_passmgr
                  pm_builder
              in
              let _ =
                Llvm_passmgr_builder.populate_module_pass_manager module_passmgr
                  pm_builder
              in
              let _ =
                Llvm_passmgr_builder.populate_lto_pass_manager lto_passmgr
                  ~internalize:true ~run_inliner:true pm_builder
              in

              (* run the optimizations *)
              let _ = Llvm.PassManager.run_module m module_passmgr in

              (* clean up *)
              let _ = Llvm.PassManager.dispose module_passmgr in
              let _ = Llvm.PassManager.dispose func_passmgr in
              let _ = Llvm.PassManager.dispose lto_passmgr in
              ()
          in

          match c with
          | Compile ->
              let _bc = Llvm_bitwriter.write_bitcode_file m (!f_name ^ ".bc") in
              let _ =
                Sys.command
                  ("llc -filetype=obj " ^ !f_name ^ ".bc -o  " ^ !f_name
                 ^ ".o && clang -no-pie " ^ !f_name ^ ".o -o " ^ !f_name)
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
