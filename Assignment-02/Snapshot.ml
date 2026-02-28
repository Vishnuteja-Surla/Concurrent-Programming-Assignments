(** Atomic Snapshot Implementation using Double-Collect Algorithm *)

(** Type of atomic snapshot object *)
type 'a t = {
  registers : 'a Atomic.t array;  (* Array of atomic registers *)
  n : int;                         (* Number of registers *)
}

(* type 'a t = {
  registers : 'a array;  (* Array of registers *)
  n : int;                         (* Number of registers *)
} *)

let create n init_value = 
  if n <= 0 then invalid_arg "The size of the array n must be greater than 0";
  let regs = Array.init n (fun _ -> Atomic.make init_value) in
  {registers = regs; n}

(* let create n init_value = 
  if n <= 0 then invalid_arg "The size of the array n must be greater than 0";
  let regs = Array.make n init_value in
  {registers = regs; n} *)

let update snapshot idx value = 
  if idx < 0 || idx >= snapshot.n then invalid_arg "Index(idx) out of bounds";
  Atomic.set snapshot.registers.(idx) value

(* let update snapshot idx value = 
  if idx < 0 || idx >= snapshot.n then invalid_arg "Index(idx) out of bounds";
  snapshot.registers.(idx) <- value *)

(** Helper: collect all register values *)
let collect snapshot =
  let c = Array.init snapshot.n (fun i -> Atomic.get snapshot.registers.(i)) in
  c

(* let collect snapshot =
  let c = Array.init snapshot.n (fun i -> snapshot.registers.(i)) in
  c *)

(** Scan using double-collect algorithm *)
let scan snapshot =
  let rec loop () =
    let c1 = collect snapshot in
    let c2 = collect snapshot in
    if c1 = c2 then c1 else loop () 
  in 
  loop ()

let size snapshot = snapshot.n