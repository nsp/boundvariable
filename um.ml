let verbose = ref false

let cprint s = if !verbose then print_string s else ()

let cprintln s = if !verbose then print_endline s else ()

type platter = int
type scroll = platter array

type um_state = {
  regs : platter array;
  scrolls : scroll array;
  finger_offset : int;
  avail_scrolls : int list
}

let default_state = {
  regs = Array.make 8 0;
  scrolls = Array.make 32 (Array.make 0 0);
  finger_offset = 0;
  avail_scrolls = [1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;
                   18;19;20;21;22;23;24;25;26;27;28;29;30;31]
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

let print_state { regs=rs; scrolls=ss; finger_offset=fo } =
  if !verbose then (
    Printf.printf   "  Finger ofs:     %08x\n" fo;
    Printf.printf   "  Registers:      %08x %08x %08x %08x\n"
      rs.(0) rs.(1) rs.(2) rs.(3);
    Printf.printf   "                  %08x %08x %08x %08x\n"
      rs.(4) rs.(5) rs.(6) rs.(7);
    if (Array.length ss.(0) - fo) > 2 then
      Printf.printf "  Current scroll: %08x %08x %08x\n"
        ss.(0).(fo) ss.(0).(succ fo) ss.(0).(succ (succ fo))
    else if (Array.length ss.(0) - fo) > 1 then
      Printf.printf "  Current scroll: %08x %08x end\n"
        ss.(0).(fo) ss.(0).(succ fo)
    else if (Array.length ss.(0) - fo) > 0 then
      Printf.printf "  Current scroll: %08x end\n" ss.(0).(fo)
    else
      Printf.printf "  Current scroll: end\n")
  else ()


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
  let { regs=rs; finger_offset=fo;
        scrolls=ss; avail_scrolls=avail_ss } = state in
  let state' = { state with finger_offset=succ fo } in
  let cont s = s, true in
  let halt s = s, false in
  cprintln "------------------------------------------------------";
  let state'', flag = match operation_of_platter ss.(0).(fo) with
  | Condmv (a,b,c) -> (cprintln "Condmv";
		       (* The register A receives the value in register B,
			  unless the register C contains 0. *)
		       if rs.(c) = 0 then cont state' else
			 let regs' = Array.copy rs in
			 regs'.(a) <- rs.(b);
			 cont {state' with regs = regs'})
  | Arridx (a,b,c) -> (cprintln (Printf.sprintf "Arridx r%d := ss[r%d[%d]][r%d[%d]]" a b rs.(b) c rs.(c));
                       (* The register A receives the value stored at offset
                          in register C in the array identified by B. *)
                       let regs' = Array.copy rs in
                       regs'.(a) <- ss.(rs.(b)).(rs.(c));
                       cont {state' with regs = regs'})
  | Arramd (a,b,c) -> (cprintln (Printf.sprintf "Arramd ss[r%d[%d][r%d[%d]] := r%d[%d]" a rs.(a) b rs.(b) c rs.(c));
		       (* The array identified by A is amended at the offset
			  in register B to store the value in register C. *)
		       let ss' = Array.copy ss in
		       let s' = Array.copy ss'.(rs.(a)) in
                       s'.(rs.(b)) <- rs.(c);
		       ss'.(rs.(a)) <- s';
                       cont {state' with scrolls = ss'})
  | Add (a,b,c)    -> (cprintln (Printf.sprintf "Add r%d := r%d[%d] + r%d[%d]" a b rs.(b) c rs.(c));
                       (* The register A receives the value in register B plus
                          the value in register C, modulo 2^32. *)
                       let regs' = Array.copy rs in
                       regs'.(a) <- (rs.(b) + rs.(c)) mod 0x100000000;
                       cont {state' with regs = regs'})
  | Mult (a,b,c)   -> (cprintln (Printf.sprintf "Mult r%d := r%d[%d] * r%d[%d]" a b rs.(b) c rs.(c));
		       (* The register A receives the value in register B times
			  the value in register C, modulo 2^32. *)
                       let regs' = Array.copy rs in
                       regs'.(a) <- (rs.(b) * rs.(c)) mod 0x100000000;
                       cont {state' with regs = regs'})
  | Div (a,b,c)    -> (cprintln (Printf.sprintf "Div r%d := r%d[%d] / r%d[%d]" a b rs.(b) c rs.(c));
                       (* The register A receives the value in register B
                          divided by the value in register C, if any, where
                          each quantity is treated treated as an unsigned 32
                          bit number. *)
                       let regs' = Array.copy rs in
                       regs'.(a) <- ((0xffffffff land rs.(b)) / (0xffffffff land rs.(c))) mod 0x100000000;
                       cont {state' with regs = regs'})
  | Nand (a,b,c)   -> (cprintln (Printf.sprintf "Nand r%d := ~(r%d[%d] /\\ r%d[%d])" a b rs.(b) c rs.(c));
		       (* Each bit in the register A receives the 1 bit if
			  either register B or register C has a 0 bit in that
			  position.  Otherwise the bit in register A receives
			  the 0 bit. *)
                       let regs' = Array.copy rs in
                       regs'.(a) <- (lnot (rs.(b) land rs.(c))) mod 0x100000000;
                       cont {state' with regs = regs'})
  | Halt           -> (cprintln "Halt";
                       (* The universal machine stops computation. *)
                       halt state')
  | Alloc (b,c)    -> ((Printf.printf "Alloc b=r%d, cap=r%d[%d]\n" b c rs.(c));
                       (* A new array is created with a capacity of platters
                          commensurate to the value in the register C. This
                          new array is initialized entirely with platters
                          holding the value 0. A bit pattern not consisting of
                          exclusively the 0 bit, and that identifies no other
                          active allocated array, is placed in the B register. *)
		       let ss' = Array.copy ss in
		       let regs' = Array.copy rs in
		       let s = Array.make rs.(c) 0 in
		       match avail_ss with
			 [] -> failwith "No scrolls available"
		       | h::t -> (ss'.(h) <- s;
				  regs'.(b) <- h;
				  cont {state' with regs=regs';
					scrolls=ss'; avail_scrolls=t}))
  | Aband (c)      -> ((Printf.printf "Aband %d\n" rs.(c));
                       (* The array identified by the register C is abandoned.
                          Future allocations may then reuse that identifier. *)
                       if rs.(c) = 0 then halt state'
                       else cont {state' with avail_scrolls = rs.(c)::avail_ss})
  | Output (c)     -> (cprint "Output ";
		       (* The value in the register C is displayed on the console
			  immediately. Only values between and including 0 and 255
			  are allowed. *)
		       print_char (char_of_int rs.(c));
		       if rs.(c) = int_of_char '\n' then flush stdout;
		       cprintln "";
                       cont state')
  | Input (c)      -> (cprintln "Input";
                       (* The universal machine waits for input on the console.
                          When input arrives, the register C is loaded with the
                          input, which must be between and including 0 and 255.
                          If the end of input has been signaled, then the
                          register C is endowed with a uniform value pattern
                          where every place is pregnant with the 1 bit. *)
		       flush stdout;
                       let regs' = Array.copy rs in
                       regs'.(c) <- (try int_of_char (input_char stdin)
                         with End_of_file -> 0x11111111);
                       cont {state' with regs = regs'})
  | Loadpr (b,c)   -> (cprintln (Printf.sprintf "Loadpr scroll %d, fo %08x" rs.(b) rs.(c));
                       (* The array identified by the B register is duplicated
                          and the duplicate shall replace the '0' array,
                          regardless of size. The execution finger is placed
                          to indicate the platter of this array that is
                          described by the offset given in C, where the value
                          0 denotes the first platter, 1 the second, et
                          cetera.

                          The '0' array shall be the most sublime choice for
                          loading, and shall be handled with the utmost
                          velocity. *)
                       if rs.(b) = 0 then cont {state' with finger_offset = rs.(c)}
                       else let ss' = Array.copy ss in
                            ss'.(0) <- ss'.(rs.(b));
                            cont {state' with scrolls=ss'; finger_offset = rs.(c)})
  | Orth (a, v)    -> (cprintln (Printf.sprintf "Orth r%d = %08x" a v);
                       (* The value indicated is loaded into the register A
                          forthwith. *)
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
  cprintln "------------------------------------------------------";
  cprintln "Program complete";
  print_state st_final;
  cprintln "------------------------------------------------------\n";

