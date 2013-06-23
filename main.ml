open Um

let main () =
  let prog_file = ref "" in
  let argspecs  =
    ["-v", Arg.Set Um.verbose, "Verbose output";
    ] in
  Arg.parse argspecs (fun p -> prog_file := p)
    "Usage: ./main [options] source_file";
    if !verbose then print_endline (Printf.sprintf "In main");
    Um.eval_prog !prog_file
;;

main ()

