(* Test.ml
 *
 * Test suite for TreeLock implementation
 * Organized into: Unit Tests, Sequential Tests, and Concurrent Tests
 *)

(** PART 1: UNIT TESTS - Testing Observable Properties **)

(* Test calculate_depth produces correct tree depth *)
let test_calculate_depth () =
  Printf.printf "Unit Test 1: Tree depth calculation...\n%!";
  (* failwith "Not implemented" *)
  let locks = TreeLock.create 2 in
    let depth = TreeLock.get_depth locks in assert (depth = 1);
  let locks = TreeLock.create 3 in
    let depth = TreeLock.get_depth locks in assert (depth = 2);
  let locks = TreeLock.create 4 in
    let depth = TreeLock.get_depth locks in assert (depth = 2);
  let locks = TreeLock.create 5 in
    let depth = TreeLock.get_depth locks in assert (depth = 3);
  let locks = TreeLock.create 6 in
    let depth = TreeLock.get_depth locks in assert (depth = 3);
  let locks = TreeLock.create 7 in
    let depth = TreeLock.get_depth locks in assert (depth = 3);
  let locks = TreeLock.create 8 in
    let depth = TreeLock.get_depth locks in assert (depth = 3);
  let locks = TreeLock.create 1 in
    let depth = TreeLock.get_depth locks in assert (depth = 0);
  Printf.printf "  ✓ Passed!\n%!"

(* Test tree structure properties *)
let test_tree_structure () =
  Printf.printf "Unit Test 2: Tree structure properties...\n%!";
  (* failwith "Not implemented" *)
  let locks = TreeLock.create 2 in
    let depth = TreeLock.get_depth locks in 
    let nodes = TreeLock.get_num_nodes locks in assert (nodes = (1 lsl depth) - 1);
  let locks = TreeLock.create 3 in
    let depth = TreeLock.get_depth locks in 
    let nodes = TreeLock.get_num_nodes locks in assert (nodes = (1 lsl depth) - 1);
  let locks = TreeLock.create 4 in
    let depth = TreeLock.get_depth locks in 
    let nodes = TreeLock.get_num_nodes locks in assert (nodes = (1 lsl depth) - 1);
  let locks = TreeLock.create 5 in
    let depth = TreeLock.get_depth locks in 
    let nodes = TreeLock.get_num_nodes locks in assert (nodes = (1 lsl depth) - 1);
  let locks = TreeLock.create 6 in
    let depth = TreeLock.get_depth locks in 
    let nodes = TreeLock.get_num_nodes locks in assert (nodes = (1 lsl depth) - 1);
  let locks = TreeLock.create 7 in
    let depth = TreeLock.get_depth locks in 
    let nodes = TreeLock.get_num_nodes locks in assert (nodes = (1 lsl depth) - 1);
  let locks = TreeLock.create 8 in
    let depth = TreeLock.get_depth locks in 
    let nodes = TreeLock.get_num_nodes locks in assert (nodes = (1 lsl depth) - 1);
  let locks = TreeLock.create 1 in
    let depth = TreeLock.get_depth locks in 
    let nodes = TreeLock.get_num_nodes locks in assert (nodes = (1 lsl depth) - 1);
  Printf.printf "  ✓ Passed!\n%!"

(* Test boundary conditions *)
(* let test_boundary_conditions () =
  Printf.printf "Unit Test 3: Boundary conditions...\n%!";
  failwith "Not implemented" *)

(** PART 2: SEQUENTIAL CORRECTNESS TESTS **)

(* Test single thread can lock/unlock *)
let test_single_thread () =
  Printf.printf "Sequential Test 1: Single thread lock/unlock...\n%!";
  (* failwith "Not implemented" *)
  let locks = TreeLock.create 2 in
    let rec loop i =
      if i >= 10 then ()
      else begin
        TreeLock.lock locks 0;
        TreeLock.unlock locks 0;
        loop (i+1)
      end
    in loop 0;
  Printf.printf "  ✓ Passed!\n%!"

(* Test multiple sequential acquisitions by different threads *)
let test_sequential_acquisitions () =
  Printf.printf "Sequential Test 2: Sequential acquisitions by multiple threads...\n%!";
  (* failwith "Not implemented" *)
  let locks = TreeLock.create 2 in
    TreeLock.lock locks 0;
    TreeLock.unlock locks 0;
    TreeLock.lock locks 1;
    TreeLock.unlock locks 1;
    TreeLock.lock locks 0;
    TreeLock.unlock locks 0;
  Printf.printf "  ✓ Passed!\n%!"

(** PART 3: CONCURRENT CORRECTNESS TESTS **)

(* Helper Functions for concurrent tests *)

let run_concurrent_test num_threads iterations = 
  let tree = TreeLock.create num_threads in
  let counter = Atomic.make 0 in
  let worker thread_id =
    for _ = 1 to iterations do
      TreeLock.lock tree thread_id;
      (* Critical Section *)
      let old_val = Atomic.get counter in
      Domain.cpu_relax ();
      Atomic.set counter (old_val + 1);
      TreeLock.unlock tree thread_id
    done
  in

  let domain_arr = Array.init num_threads (fun i -> Domain.spawn(fun () -> worker i)) in
  Array.iter Domain.join domain_arr;

  let final = Atomic.get counter in
  let expected = num_threads * iterations in
  if final = expected then
    Printf.printf "  ✓ Passed: counter = %d (expected %d)\n%!" final expected
  else
    Printf.printf "  ✗ FAILED: counter = %d (expected %d)\n%!" final expected


(* Convert thread ID to path in the tree *)

(* Test 1: Basic functionality with 2 threads *)
let test_two_threads () =
  Printf.printf "Concurrent Test 1: Two threads...\n%!";
  let tree = TreeLock.create 2 in
  let counter = Atomic.make 0 in
  let iterations = 1000 in

  let worker thread_id =
    for _ = 1 to iterations do
      TreeLock.lock tree thread_id;
      (* Critical section *)
      let old_val = Atomic.get counter in
      Domain.cpu_relax (); (* Introduce some delay to test race conditions *)
      Atomic.set counter (old_val + 1);
      TreeLock.unlock tree thread_id
    done
  in

  let d1 = Domain.spawn (fun () -> worker 0) in
  let d2 = Domain.spawn (fun () -> worker 1) in

  Domain.join d1;
  Domain.join d2;

  let final = Atomic.get counter in
  let expected = 2 * iterations in
  if final = expected then
    Printf.printf "  ✓ Passed: counter = %d (expected %d)\n%!" final expected
  else
    Printf.printf "  ✗ FAILED: counter = %d (expected %d)\n%!" final expected

(* Test 2: Four threads *)
let test_four_threads () =
  (* failwith "Not implemented" *)
  Printf.printf "Concurrent Test 2: Four threads...\n%!";
  run_concurrent_test 4 1000

(* Test 3: Eight threads *)
let test_eight_threads () =
  (* failwith "Not implemented" *)
  Printf.printf "Concurrent Test 3: Eight threads...\n%!";
  run_concurrent_test 8 1000

(* Test 4: Non-power-of-two threads (5 threads) *)
let test_five_threads () =
  (* failwith "Not implemented" *)
  Printf.printf "Concurrent Test 4: Five threads...\n%!";
  run_concurrent_test 5 1000

(* Test 5: Stress test - multiple increments per critical section *)
let test_stress () =
  (* failwith "Not implemented" *)
  Printf.printf "Concurrent Test 5: Stress test with multiple increments...\n%!";
  let tree = TreeLock.create 8 in
  let iterations = 1000 in
  let x = Atomic.make 0 in
  let y = Atomic.make 0 in
  
  let worker thread_id =
    for _ = 1 to iterations do
      TreeLock.lock tree thread_id;
      (* Critical Section *)
      let old_val = Atomic.get x in
      Atomic.set x (old_val + 1);
      Domain.cpu_relax ();
      let old_val = Atomic.get y in
      Atomic.set y (old_val + 1);
      assert(Atomic.get x = Atomic.get y);
      TreeLock.unlock tree thread_id
    done
  in

  let domain_arr = Array.init 8 (fun i -> Domain.spawn(fun () -> worker i)) in
  Array.iter Domain.join domain_arr;
  Printf.printf "  ✓ Passed!\n%!"

(* Test 6: Tree structure verification *)
let test_structure_verification () =
  (* failwith "Not implemented" *)
  Printf.printf "Sanity Check: Tree Structure Verification\n%!";
  let tree = TreeLock.create 8 in
  assert(TreeLock.get_depth tree = 3);
  assert(TreeLock.get_num_nodes tree = 7);
  run_concurrent_test 8 100;  
  assert(TreeLock.get_depth tree = 3);
  assert(TreeLock.get_num_nodes tree = 7);
  Printf.printf "✓ Passed: Tree Structure is Consistent\n%!"

(* Test 7: Performance benchmark *)
let test_performance () =
  (* failwith "Not implemented" *)
  Printf.printf "Performance Test: 8 threads, 10000 iterations...\n%!";
  for i = 1 to 5 do
    if (i <> 3) then begin
    let start = Sys.time () in
    run_concurrent_test i (10000/i);
    let stop = Sys.time () in
    Printf.printf "Execution Time: %fs\n%!" (stop -. start)
    end
  done;
  let start = Sys.time () in
  run_concurrent_test 8 (10000/8);
  let stop = Sys.time () in
  Printf.printf "Execution Time: %fs\n%!" (stop -. start)

(* Main test runner *)
let () =
  Printf.printf "=== TreeLock Test Suite ===\n\n%!";

  TreeLock.print_tree_info (TreeLock.create 8);
  Printf.printf "\n%!";

  (* Unit Tests *)
  Printf.printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n%!";
  Printf.printf "PART 1: UNIT TESTS\n%!";
  Printf.printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n%!";
  test_calculate_depth ();
  Printf.printf "\n%!";
  test_tree_structure ();
  Printf.printf "\n%!";
  (* test_boundary_conditions (); *)
  Printf.printf "\n%!";

  (* Sequential Tests *)
  Printf.printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n%!";
  Printf.printf "PART 2: SEQUENTIAL CORRECTNESS\n%!";
  Printf.printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n%!";
  test_single_thread ();
  Printf.printf "\n%!";
  test_sequential_acquisitions ();
  Printf.printf "\n%!";

  (* Concurrent Tests *)
  Printf.printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n%!";
  Printf.printf "PART 3: CONCURRENT CORRECTNESS\n%!";
  Printf.printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n%!";
  test_two_threads ();
  test_four_threads ();
  test_eight_threads ();
  test_five_threads ();
  test_stress ();
  test_structure_verification ();
  test_performance ();

  Printf.printf "\n=== Test Suite Complete ===\n%!"
