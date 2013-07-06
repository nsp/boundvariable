type platter = int
type scroll = platter array

type um_state = {
  regs : platter array;
  scrolls : scroll array;
  finger_offset : int * int (* array, offset *)
}

let default_state = {
  regs = Array.make 8 0;
  scrolls = Array.make 32 (Array.make 0 0);
  finger_offset = 0,0
}

(* unwind and with_* functions from http://stackoverflow.com/a/11278170/34910 *)
let unwind ~protect f x =
  let module E = struct type 'a t = Left of 'a | Right of exn end in
  let res = try E.Left (f x) with e -> E.Right e in
  let ()  = protect x in
  match res with
  | E.Left  y -> y
  | E.Right e -> raise e

let with_input_channel inch f =
  unwind ~protect:close_in f inch

let with_input_file fname =
  with_input_channel (open_in_bin fname)

(** [init_um f] initializes universal machine with contents of
    scroll 0 taken from file [f]**)
let init_um name : um_state =
  let read_scroll_from_chan chan =
    let len = (in_channel_length chan) / 4 in
    let scrl = Array.make len 0 in
    let scrls = Array.copy default_state.scrolls in
    (for i = 0 to pred len do
        scrl.(i) <- 0xffffffff land (input_binary_int chan)
    done);
    scrls.(0) <- scrl;
    { default_state with scrolls = scrls }
  in
  with_input_file name read_scroll_from_chan

let print_state { regs=rs; scrolls=ss; finger_offset=(fs,fo) } =
  Printf.printf "  Finger scr/ofs: %08x/%08x\n" fs fo;
  Printf.printf "  Registers:      %08x %08x %08x %08x\n"
    rs.(0) rs.(1) rs.(2) rs.(3);
  Printf.printf "                  %08x %08x %08x %08x\n"
    rs.(4) rs.(5) rs.(6) rs.(7);
  if (Array.length ss.(fs) - fo) > 2 then
    Printf.printf "  Current scroll: %08x %08x %08x\n" ss.(fs).(fo) ss.(fs).(succ fo) ss.(fs).(succ (succ fo))
  else if (Array.length ss.(fs) - fo) > 1 then
    Printf.printf "  Current scroll: %08x %08x end\n" ss.(fs).(fo) ss.(fs).(succ fo)
  else if (Array.length ss.(fs) - fo) > 0 then
    Printf.printf "  Current scroll: %08x end\n" ss.(fs).(fo)
  else
    Printf.printf "  Current scroll: end\n";


(***********************************************************)

type reg = int (* 3-bit # *)
type value = int

type operation =
  | Condmv of reg * reg * reg
  | Arridx of reg * reg * reg
  | Arramd of reg * reg * reg
  | Add of reg * reg * reg
  | Mult of reg * reg * reg
  | Div of reg * reg * reg
  | Nand of reg * reg * reg
  | Halt
  | Alloc of reg * reg
  | Aband of reg
  | Output of reg
  | Input of reg
  | Loadpr of reg * reg
  | Orth of reg * value

(*** Constants ***)

let opcode_shift = 4+3*8
let areg_shift = 6
let breg_shift = 3
let creg_shift = 0
let int_mod = 4294967296

(*** operation of platter ***)

let operation_of_platter pl : operation =
  let get_reg shift = (pl lsr shift) land 0x7 in
  let a = get_reg areg_shift in
  let b = get_reg breg_shift in
  let c = get_reg creg_shift in
  let opcode = pl lsr opcode_shift in
  match opcode with
     0 -> Condmv (a,b,c)
  |  1 -> Arridx (a,b,c)
  |  2 -> Arramd (a,b,c)
  |  3 -> Add    (a,b,c)
  |  4 -> Mult   (a,b,c)
  |  5 -> Div    (a,b,c)
  |  6 -> Nand   (a,b,c)
  |  7 -> Halt
  |  8 -> Alloc  (b,c)
  |  9 -> Aband  (c)
  | 10 -> Output (c)
  | 11 -> Input  (c)
  | 12 -> Loadpr (b,c)
  | 13 -> Orth   ((pl lsr 25) land 7, (pl land 0x1ffffff))
  | _ -> failwith (Printf.sprintf "Invalid opcode (%d) in platter (%08x)" opcode pl)

(*** platter_of_operation functions ***)

let platter_of_operation : operation -> platter =
  let set_areg v pl = pl lor ((v land 7) lsl areg_shift) in
  let set_breg v pl = pl lor ((v land 7) lsl breg_shift) in
  let set_creg v pl = pl lor ((v land 7) lsl creg_shift) in
  let std_op opcode a b c = set_areg a (set_breg b (set_creg c (opcode lsl opcode_shift))) in
  function
  | Condmv (a,b,c) -> std_op  0 a b c
  | Arridx (a,b,c) -> std_op  1 a b c
  | Arramd (a,b,c) -> std_op  2 a b c
  | Add (a,b,c)    -> std_op  3 a b c
  | Mult (a,b,c)   -> std_op  4 a b c
  | Div (a,b,c)    -> std_op  5 a b c
  | Nand (a,b,c)   -> std_op  6 a b c
  | Halt           -> std_op  7 0 0 0
  | Alloc (b,c)    -> std_op  8 0 b c
  | Aband (c)      -> std_op  9 0 0 c
  | Output (c)     -> std_op 10 0 0 c
  | Input (c)      -> std_op 11 0 0 c
  | Loadpr (b,c)   -> std_op 12 0 b c
  | Orth (a, v)    -> (13 lsl opcode_shift) lor ((a land 7) lsl 25) lor (v land 0x01ffffff)

let make_scroll ops : scroll =
  Array.of_list (List.map platter_of_operation ops)

let make_state ops =
  let scrls = Array.copy default_state.scrolls in
  scrls.(0) <- make_scroll ops;
  { default_state with scrolls = scrls }

(*** Spin cycling **)

(** [do_spin_cycle s] performs a single spin cycle, returns
    the resulting state and  flag (false=halted) **)
let do_spin_cycle state : um_state * bool =
  let { regs=rs; scrolls=ss; finger_offset=(fs, fo) } = state in
  let state' = { state with finger_offset=(fs, succ fo) } in
  let cont s = s, true in
  let halt s = s, false in
  print_endline "------------------------------------------------------";
  let state'', flag = match operation_of_platter ss.(fs).(fo) with
  | Condmv (a,b,c) ->
    (
      print_endline "Condmv";

      (* The register A receives the value in register B, *)
      (* unless the register C contains 0. *)
      let regs' = Array.copy rs in
      regs'.(a) <- if (regs'.(c) = 0) then regs'.(a) else regs'.(b);

      cont { state' with regs = regs' }
    )
  | Arridx (a,b,c) -> (print_endline "Arridx";
                      cont state')
  | Arramd (a,b,c) ->

    (
      print_endline "Arramd";
      print_endline (Printf.sprintf "Arramd ss[%d][%d] := %d\n" a rs.(b) rs.(c));

      let regs' = Array.copy rs in
      let ss' = Array.copy ss in
      ss'.(regs'.(a)).(regs'.(b)) <- regs'.(c);
      (* The array identified by A is amended at the offset *)
      (* in register B to store the value in register C. *)

      cont {{ state' with scrolls = ss' } with regs = regs' }
    )

  | Add (a,b,c)    -> (print_endline "Add";
           cont state')
  | Mult (a,b,c)   ->
    (
      print_endline "Mult";

      (* reg A gets reg B x reg C modulo 2^32 *)
      let regs' = Array.copy rs in
      regs'.(a) <- (regs'.(b) * regs'.(c)) mod int_mod;

      cont { state' with regs = regs' }
    )

  | Div (a,b,c)    -> (print_endline "Div";
		       cont state')
  | Nand (a,b,c)   -> (print_endline "Nand";
		       cont state')
  | Halt           -> (print_endline "Halt";
		       halt state')
  | Alloc (b,c)    -> (print_endline "Alloc";
		       cont state')
  | Aband (c)      -> (print_endline "Aband";
           cont state')
  | Output (c)     ->

    (
      print_endline "Output";

      (* Value in reg C is printed to the console *)
      (* Only values 0 - 255 are alllowed *)
      if (rs.(c) >= 0 && rs.(c) <= 255)
      then (print_int rs.(c); print_newline ());

      cont state'
    )

  | Input (c)      -> (print_endline "Input";
           cont state')
  | Loadpr (b,c)   ->

    (
      print_endline "Loadpr";

      (* The array identified by the B register is duplicated *)
      (* and the duplicate shall replace the '0' array, *)
      (* regardless of size. The execution finger is placed *)
      (* to indicate the platter of this array that is *)
      (* described by the offset given in C, where the value *)
      (* 0 denotes the first platter, 1 the second, et *)
      (* cetera. *)

      (* The '0' array shall be the most sublime choice for *)
      (* loading, and shall be handled with the utmost *)
      (* velocity. *)

      cont state'
    )

  | Orth (a, v)    -> (print_endline "Orth";
           let regs' = Array.copy rs in
           regs'.(a) <- v;
           cont {state' with regs = regs'})
  in print_state state; state'', flag

let eval_prog f =
  let rec spin st =
    let st', cont = do_spin_cycle st in
    if cont then spin st' else st'
  in
  let init_state = init_um f in
  let st_final = spin init_state in
  print_endline "------------------------------------------------------";
  print_endline "Program complete";
  print_state st_final;
  print_endline "------------------------------------------------------\n";

