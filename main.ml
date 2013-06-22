open Um

let main () =
  let verbose   = ref false in
  let prog_file = ref "" in
  let argspecs  =
    ["-v", Arg.Set verbose, "Verbose output";
    ] in
  Arg.parse argspecs (fun p -> prog_file := p)
    "Usage: ./main [options] source_file";
    if !verbose then print_endline (Printf.sprintf "In main");
    let init_state = init_um !prog_file in
    if !verbose then (print_endline "Program loaded"; print_state init_state)
;;

main ()

