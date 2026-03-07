(** Atomic Snapshot Implementation using The Gang of Six Algorithm *)

(** Type of Register State *)
type 'a register_state = {
  seq : int;            (* Version of the current register value *)
  value : 'a;             (* Value of the register *)
  view : 'a array;      (* Stored view of the register *)
}

(** Type of atomic snapshot object *)
type 'a t = {
  register_states : 'a register_state Atomic.t array;  (* Atomic array of Register states *)
  n : int;                        (* Number of registers *)
}

let create n init_value =
  if n <= 0 then invalid_arg "The size of the array n must be greater than 0";
  let initial_view = Array.make n init_value in
  let default_state = {seq = 0; value = init_value; view = initial_view} in
  let reg_states = Array.init n (fun _ -> Atomic.make default_state) in
  {register_states = reg_states; n}

(** Helper: Collect all the register states *)
let collect snapshot =
  let rs = Array.init snapshot.n (fun i -> Atomic.get snapshot.register_states.(i)) in
  rs

let scan snapshot =
  let n = snapshot.n in
  let moved = Array.make n false in

  let rec loop prev_collect =

    (* Get the current snapshot *)
    let current_collect = collect snapshot in

    (* Check if someone moved twice *)
      let rec check_double_move i =
        if i = n then None
        else if prev_collect.(i).seq <> current_collect.(i).seq && moved.(i) then
          Some current_collect.(i).view
        else
          check_double_move (i+1)
      in

      match check_double_move 0 with
      | Some borrowed_view -> borrowed_view
      | None ->
        (* Check if someone moved once *)
          let someone_moved = ref false in
          for i = 0 to n - 1 do
            if prev_collect.(i).seq <> current_collect.(i).seq then begin
              moved.(i) <- true;
              someone_moved := true
            end
          done;

          if not !someone_moved then
            Array.init n (fun i -> current_collect.(i).value)
          else
            loop current_collect
    in
    loop (collect snapshot)

let size snapshot = snapshot.n

let update snapshot idx value =
  if idx < 0 || idx >= snapshot.n then invalid_arg "Index(idx) out of bounds";

  let curr_state = Atomic.get snapshot.register_states.(idx) in
  let curr_seq = curr_state.seq in
  let curr_view = scan snapshot in
  let new_state = {
    seq = (curr_seq + 1);
    value;
    view = curr_view; 
  } in
  Atomic.set snapshot.register_states.(idx) new_state