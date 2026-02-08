(* TreeLock.ml
 *
 * Tree-based lock implementation for n-thread mutual exclusion
 * Uses Peterson locks at each internal node of a binary tree
 *)

type t = {
  nodes: PetersonNode.t array;
  depth: int;
  num_threads: int
} (* Define this yourself *)

(* Calculate the depth of the tree needed for n threads *)
let calculate_depth n =
  (* Depth = ceiling(log2(n)) *)  
  (* failwith "Not implemented" *)
  if n <= 0 then invalid_arg "Number of threads must be positive";
  Float.ceil (Float.log2 (float_of_int n)) |> int_of_float

(* Convert thread_id to binary path representation *)
let thread_id_to_path thread_id depth =
  (* Returns array of 0s and 1s representing path from root to leaf *)
  (* failwith "Not implemented" *)
  (* let arr = Array.make depth 0 in
    let rec loop i = 
      if i >= depth then ()
      else begin
        arr.(i) <- (thread_id lsr (depth - i - 1)) land 1;
        loop (i + 1)
      end
    in loop 0;
    arr *)
    Array.init depth (fun i -> (thread_id lsr (depth-i-1)) land 1)


(* Get index of node in array given path from root *)
let path_to_index path level =
  (* Level 0 is root (index 0)
     Left child of i is 2*i + 1
     Right child of i is 2*i + 2 *)
  (* failwith "Not implemented" *)
  let idx = ref 0 in
    let rec loop i =
      if i >= level then ()
      else begin
        if path.(i) = 1 then idx := 2 * !idx + 2 else idx := 2 * !idx + 1;
        loop (i+1)
      end
    in loop 0;
  !idx

let create num_threads =
  (* failwith "Not implemented" *)
  let depth = calculate_depth num_threads in
    let size = ((1 lsl depth) - 1) in
      let arr = Array.init size (fun _ -> PetersonNode.create()) in
      {nodes = arr; depth; num_threads}

let lock tree thread_id =
  (* failwith "Not implemented" *)
  let path = thread_id_to_path thread_id tree.depth in
    let rec loop i =
      if i < 0 then ()
      else begin
        let idx = path_to_index path i in
          let node = tree.nodes.(idx) in
          PetersonNode.lock node path.(i);
        loop (i-1)
      end
    in loop (tree.depth - 1)

let unlock tree thread_id =
  (* failwith "Not implemented" *)
  let path = thread_id_to_path thread_id tree.depth in
    let rec loop i =
      if i >= tree.depth then ()
      else begin
        let idx = path_to_index path i in
          let node = tree.nodes.(idx) in
          PetersonNode.unlock node path.(i);
        loop (i+1)
      end
    in loop 0

(* Additional utility functions for debugging and analysis *)

let get_depth tree = tree.depth
  (* failwith "Not implemented" *)

let get_num_nodes tree = Array.length tree.nodes
  (* failwith "Not implemented" *)

let print_tree_info tree =
  (* failwith "Not implemented" *)
  Printf.printf "Depth: %d, Nodes: %d\n%!" tree.depth (Array.length tree.nodes)
