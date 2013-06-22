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
  Printf.printf "---------------------------------------\n";
  Printf.printf "Finger scroll/offset: %08x/%08x\n" fs fo;
  Printf.printf "Registers:            %08x %08x %08x %08x %08x %08x %08x %08x\n"
    rs.(0) rs.(1) rs.(2) rs.(3) rs.(4) rs.(5) rs.(6) rs.(7);
  if (Array.length ss.(fs) - fo) > 2 then
    Printf.printf "Current scroll:       %08x %08x %08x\n" ss.(fs).(fo) ss.(fs).(succ fo) ss.(fs).(succ (succ fo))
  else if (Array.length ss.(fs) - fo) > 1 then
    Printf.printf "Current scroll:       %08x %08x\n" ss.(fs).(fo) ss.(fs).(succ fo)
  else if (Array.length ss.(fs) - fo) > 0 then
    Printf.printf "Current scroll:       %08x\n" ss.(fs).(fo)
  else
    Printf.printf "Reached end of the scroll\n"

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

(*** util fns ***)

let get_reg shift pl =
  (pl lsr shift) land 0x7

let get_areg = get_reg areg_shift

let get_breg = get_reg breg_shift

let get_creg = get_reg creg_shift

(*** operation of platter ***)

let operation_of_platter pl : operation =
  let opcode = pl lsr opcode_shift in
  match opcode with
    0 -> Halt
  | 1 -> Halt
  | 2 -> Halt
  | 3 -> Halt
  | 4 -> Halt
  | 5 -> Halt
  | 6 -> Halt
  | 7 -> Halt
  | 8 -> Halt
  | 9 -> Halt
  | 10 -> Halt
  | 11 -> Halt
  | 12 -> Halt
  | 13 -> Halt
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
  match operation_of_platter ss.(fs).(fo) with
  | Halt -> print_endline "Halting"; halt state'
  | _ -> failwith (Printf.sprintf "Operation not implemented, platter=%x" ss.(fs).(fo))

let eval_prog f =
  let rec spin st =
    print_state st;
    let st', cont = do_spin_cycle st in
    if cont then spin st' else st'
  in
  let init_state = init_um f in
  let st_final = spin init_state in
  print_endline "----------------\nProgram complete";
  print_state st_final;
  print_endline "------------------------------------------------\n"
